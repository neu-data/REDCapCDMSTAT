<?php

namespace Neudata\CDMSTATDRW;

use ExternalModules\AbstractExternalModule;

class ApiException extends \Exception
{
}

/**
 * REDCap Data Resolution Workflow (DRW) over the REDCap API.
 *
 * REDCap has no public API for the DRW, so this module reads and writes the
 * same two tables the DRW pop-up uses: redcap_data_quality_status (one row per
 * record/event/field/instance thread, non_rule = 1 for field queries) and
 * redcap_data_quality_resolutions (one row per comment / status change).
 * Before any write the module verifies that the DRW is enabled, that both
 * tables have the columns it expects, and that the token user holds the DRW
 * right needed for the action, so a REDCap upgrade that changes the schema
 * makes the module refuse rather than corrupt data.
 */
class CDMSTATDRW extends AbstractExternalModule
{
    const QUERY_STATUSES = ['OPEN', 'CLOSED', 'VERIFIED', 'DEVERIFIED'];
    const RESPONSES = ['DATA_MISSING', 'TYPOGRAPHICAL_ERROR', 'CONFIRMED_CORRECT', 'WRONG_SOURCE', 'OTHER'];

    const STATUS_COLUMNS = ['status_id', 'non_rule', 'project_id', 'record', 'event_id', 'field_name',
        'repeat_instrument', 'instance', 'query_status', 'assigned_user_id'];
    const RESOLUTION_COLUMNS = ['res_id', 'status_id', 'ts', 'user_id', 'response_requested', 'response',
        'comment', 'current_query_status'];

    // User-rights values of data_quality_resolution:
    // 0 none, 1 view, 4 open only, 2 respond only, 5 open + respond, 3 open + close + respond.
    const CAPABILITIES = [
        'view' => [1, 2, 3, 4, 5],
        'open' => [3, 4, 5],
        'respond' => [2, 3, 5],
        'close' => [3],
        'verify' => [3],
    ];

    const ACTION_NEEDS = [
        'drw-status' => null,
        'list-queries' => 'view',
        'open-query' => 'open',
        'respond' => 'respond',
        'close-query' => 'close',
        'reopen-query' => 'open',
        'add-comment' => 'respond',
        'verify-field' => 'verify',
        'deverify-field' => 'verify',
        'lock-form' => null,
        'unlock-form' => null,
        'list-locks' => null,
    ];

    public function redcap_module_api($action, $payload, $project_id, $user_id, $format, $returnFormat, $csvDelim)
    {
        try {
            if ($returnFormat !== null && $returnFormat !== '' && strtolower($returnFormat) !== 'json') {
                throw new ApiException('Only returnFormat=json is supported.', 400);
            }
            if (empty($project_id) || empty($user_id)) {
                throw new ApiException('A project API token is required.', 401);
            }
            if (!array_key_exists($action, self::ACTION_NEEDS)) {
                throw new ApiException("Unknown action '$action'.", 404);
            }
            $payload = is_array($payload) ? $payload : [];
            $project_id = (int)$project_id;
            if ($action === 'drw-status') {
                return $this->ok($this->status($project_id, $user_id));
            }
            if (in_array($action, ['lock-form', 'unlock-form', 'list-locks'], true)) {
                $this->assertLockReady($project_id, $user_id, $action !== 'list-locks');
                if ($action === 'lock-form') {
                    return $this->ok($this->lockForm($project_id, $user_id, $payload));
                }
                if ($action === 'unlock-form') {
                    return $this->ok($this->unlockForm($project_id, $user_id, $payload));
                }
                return $this->ok($this->listLocks($project_id, $payload));
            }
            $this->assertReady($project_id, $user_id, self::ACTION_NEEDS[$action]);
            switch ($action) {
                case 'list-queries':
                    return $this->ok($this->listQueries($project_id, $payload));
                case 'open-query':
                    return $this->ok($this->openQuery($project_id, $user_id, $payload));
                case 'respond':
                    return $this->ok($this->transition($project_id, $user_id, $payload, 'respond'));
                case 'close-query':
                    return $this->ok($this->transition($project_id, $user_id, $payload, 'close'));
                case 'reopen-query':
                    return $this->ok($this->transition($project_id, $user_id, $payload, 'reopen'));
                case 'add-comment':
                    return $this->ok($this->transition($project_id, $user_id, $payload, 'comment'));
                case 'verify-field':
                    return $this->ok($this->verify($project_id, $user_id, $payload, 'VERIFIED'));
                case 'deverify-field':
                    return $this->ok($this->verify($project_id, $user_id, $payload, 'DEVERIFIED'));
            }
            throw new ApiException("Unhandled action '$action'.", 501);
        } catch (ApiException $e) {
            return $this->error($e->getMessage(), $e->getCode() ?: 400);
        } catch (\Throwable $e) {
            $this->logError($e);
            return $this->error('Internal error: ' . $e->getMessage(), 500);
        }
    }

    // ---- responses ---------------------------------------------------------

    protected function ok($data)
    {
        return ['status' => 200, 'body' => json_encode($data)];
    }

    protected function error($message, $status)
    {
        return ['status' => (int)$status, 'body' => json_encode(['error' => $message])];
    }

    // ---- readiness checks --------------------------------------------------

    public function status($project_id, $username)
    {
        $missing = $this->missingColumns();
        $level = $this->rightsLevel($project_id, $username);
        $caps = [];
        foreach (self::CAPABILITIES as $cap => $levels) {
            $caps[$cap] = $level === 'superuser' || in_array($level, $levels, true);
        }
        return [
            'module' => 'cdmstat_drw',
            'version' => '1.0.0',
            'drw_enabled' => $this->drwEnabled($project_id),
            'schema_ok' => empty($missing),
            'missing_columns' => $missing,
            'user' => $username,
            'rights_level' => $level,
            'capabilities' => $caps,
            'lock_right' => $this->safely(function () use ($project_id, $username) { return $this->lockRight($project_id, $username); }),
            'locking_schema_ok' => empty(array_diff(self::LOCK_COLUMNS, $this->tableColumns('redcap_locking_data'))),
        ];
    }

    protected function assertReady($project_id, $username, $capability)
    {
        if (!$this->drwEnabled($project_id)) {
            throw new ApiException('The Data Resolution Workflow is not enabled in this project (Project Setup > Additional customizations).', 403);
        }
        $missing = $this->missingColumns();
        if (!empty($missing)) {
            throw new ApiException('Unexpected DRW database schema; refusing to write. Missing: ' . implode(', ', $missing), 501);
        }
        $level = $this->rightsLevel($project_id, $username);
        if ($level !== 'superuser' && !in_array($level, self::CAPABILITIES[$capability], true)) {
            throw new ApiException("User '$username' lacks the Data Resolution Workflow right to $capability.", 403);
        }
        if ($capability !== 'view') {
            $allowed = trim((string)$this->getProjectSetting('allowed-usernames', $project_id));
            if ($allowed !== '' && !in_array($username, array_map('trim', explode(',', $allowed)), true)) {
                throw new ApiException("User '$username' is not allowed to write through this module.", 403);
            }
        }
    }

    protected function drwEnabled($project_id)
    {
        $row = $this->one('select data_resolution_enabled from redcap_projects where project_id = ?', [$project_id]);
        return $row !== null && (int)$row['data_resolution_enabled'] === 2;
    }

    protected function missingColumns()
    {
        $missing = [];
        foreach (['redcap_data_quality_status' => self::STATUS_COLUMNS,
                  'redcap_data_quality_resolutions' => self::RESOLUTION_COLUMNS] as $table => $cols) {
            $have = $this->tableColumns($table);
            foreach ($cols as $c) {
                if (!in_array($c, $have, true)) {
                    $missing[] = "$table.$c";
                }
            }
        }
        return $missing;
    }

    protected function tableColumns($table)
    {
        $res = $this->query(
            'select column_name as c from information_schema.columns where table_schema = database() and table_name = ?',
            [$table]
        );
        $cols = [];
        while ($r = $res->fetch_assoc()) {
            $cols[] = strtolower($r['c'] ?? $r['COLUMN_NAME'] ?? '');
        }
        return $cols;
    }

    protected function rightsLevel($project_id, $username)
    {
        $su = $this->one('select super_user from redcap_user_information where username = ?', [$username]);
        if ($su !== null && (int)$su['super_user'] === 1) {
            return 'superuser';
        }
        $row = $this->one(
            'select u.data_quality_resolution as own, r.data_quality_resolution as role
               from redcap_user_rights u left join redcap_user_roles r on r.role_id = u.role_id
              where u.project_id = ? and u.username = ?',
            [$project_id, $username]
        );
        if ($row === null) {
            return 0;
        }
        return (int)($row['role'] !== null ? $row['role'] : $row['own']);
    }

    // ---- reading -----------------------------------------------------------

    public function listQueries($project_id, $payload)
    {
        $since = (int)($payload['since_res_id'] ?? 0);
        $sql = 'select s.status_id, s.record, s.event_id, s.field_name, s.instance, s.repeat_instrument, s.query_status,
                       ua.username as assigned_to
                  from redcap_data_quality_status s
                  left join redcap_user_information ua on ua.ui_id = s.assigned_user_id
                 where s.project_id = ? and s.non_rule = 1';
        $params = [$project_id];
        if (!empty($payload['record'])) {
            $sql .= ' and s.record = ?';
            $params[] = (string)$payload['record'];
        }
        if ($since > 0) {
            $sql .= ' and exists (select 1 from redcap_data_quality_resolutions x where x.status_id = s.status_id and x.res_id > ?)';
            $params[] = $since;
        }
        $sql .= ' order by s.status_id';
        $threads = [];
        $res = $this->query($sql, $params);
        while ($r = $res->fetch_assoc()) {
            $r['event'] = $this->uniqueEventName($project_id, (int)$r['event_id']);
            $r['resolutions'] = [];
            $threads[(int)$r['status_id']] = $r;
        }
        if (!empty($threads)) {
            $q = $this->createQuery();
            $q->add('select d.res_id, d.status_id, d.ts, ui.username, d.response_requested, d.response, d.comment, d.current_query_status
                       from redcap_data_quality_resolutions d left join redcap_user_information ui on ui.ui_id = d.user_id where');
            $q->addInClause('d.status_id', array_keys($threads));
            $q->add('order by d.res_id');
            $rr = $q->execute();
            while ($r = $rr->fetch_assoc()) {
                $threads[(int)$r['status_id']]['resolutions'][] = $r;
            }
        }
        $max = $this->one('select max(d.res_id) as m from redcap_data_quality_resolutions d
                             join redcap_data_quality_status s on s.status_id = d.status_id where s.project_id = ?', [$project_id]);
        return ['threads' => array_values($threads), 'max_res_id' => (int)($max['m'] ?? 0)];
    }

    // ---- writing -----------------------------------------------------------

    public function openQuery($project_id, $username, $payload)
    {
        [$record, $event_id, $field, $instance, $repeat] = $this->target($project_id, $payload);
        $comment = $this->requireText($payload, 'comment');
        $assign = null;
        if (!empty($payload['assign_to'])) {
            $assign = $this->uiId((string)$payload['assign_to']);
            if ($assign === null) {
                throw new ApiException("Unknown user to assign: {$payload['assign_to']}", 400);
            }
        }
        $uid = $this->uiId($username);
        $this->begin();
        try {
            $existing = $this->thread($project_id, $record, $event_id, $field, $instance, $repeat);
            if ($existing !== null && $existing['query_status'] === 'OPEN') {
                $this->rollback();
                return ['status_id' => (int)$existing['status_id'], 'query_status' => 'OPEN', 'already_open' => true];
            }
            if ($existing === null) {
                $this->query(
                    'insert into redcap_data_quality_status
                       (non_rule, project_id, record, event_id, field_name, repeat_instrument, instance, query_status, assigned_user_id)
                     values (1, ?, ?, ?, ?, ?, ?, ?, ?)',
                    [$project_id, $record, $event_id, $field, $repeat, $instance, 'OPEN', $assign]
                );
                $status_id = $this->lastInsertId();
            } else {
                $status_id = (int)$existing['status_id'];
                $this->query('update redcap_data_quality_status set query_status = ?, assigned_user_id = ? where status_id = ?',
                    ['OPEN', $assign, $status_id]);
            }
            $res_id = $this->addResolution($status_id, $uid, 1, null, $this->attribute($payload, $comment, $project_id), 'OPEN');
            $this->commit();
        } catch (\Throwable $e) {
            $this->rollback();
            throw $e;
        }
        $this->audit($project_id, $record, $event_id, 'Open data query', "field = $field, instance = $instance, status_id = $status_id");
        return ['status_id' => $status_id, 'res_id' => $res_id, 'query_status' => 'OPEN', 'already_open' => false];
    }

    public function transition($project_id, $username, $payload, $kind)
    {
        $status_id = (int)($payload['status_id'] ?? 0);
        $comment = $this->requireText($payload, 'comment');
        $t = $this->one('select * from redcap_data_quality_status where status_id = ? and project_id = ? and non_rule = 1',
            [$status_id, $project_id]);
        if ($t === null) {
            throw new ApiException("No query thread $status_id in this project.", 404);
        }
        $current = $t['query_status'];
        $response = null;
        $requested = 0;
        switch ($kind) {
            case 'respond':
                if ($current !== 'OPEN') {
                    throw new ApiException("Only open queries can be answered (status is $current).", 409);
                }
                $response = strtoupper((string)($payload['response'] ?? 'OTHER'));
                if (!in_array($response, self::RESPONSES, true)) {
                    throw new ApiException('response must be one of ' . implode(', ', self::RESPONSES), 400);
                }
                $new = 'OPEN';
                break;
            case 'close':
                if ($current !== 'OPEN') {
                    throw new ApiException("Only open queries can be closed (status is $current).", 409);
                }
                $new = 'CLOSED';
                break;
            case 'reopen':
                if ($current !== 'CLOSED') {
                    throw new ApiException("Only closed queries can be reopened (status is $current).", 409);
                }
                $new = 'OPEN';
                $requested = 1;
                break;
            default:
                $new = $current;
        }
        $uid = $this->uiId($username);
        $this->begin();
        try {
            if ($new !== $current) {
                $this->query('update redcap_data_quality_status set query_status = ? where status_id = ?', [$new, $status_id]);
            }
            $res_id = $this->addResolution($status_id, $uid, $requested, $response, $this->attribute($payload, $comment, $project_id), $new);
            $this->commit();
        } catch (\Throwable $e) {
            $this->rollback();
            throw $e;
        }
        $labels = ['respond' => 'Respond to data query', 'close' => 'Close data query', 'reopen' => 'Reopen data query',
                   'comment' => 'Comment on data query'];
        $this->audit($project_id, $t['record'], (int)$t['event_id'], $labels[$kind],
            "field = {$t['field_name']}, status_id = $status_id" . ($response ? ", response = $response" : ''));
        return ['status_id' => $status_id, 'res_id' => $res_id, 'query_status' => $new];
    }

    public function verify($project_id, $username, $payload, $status)
    {
        [$record, $event_id, $field, $instance, $repeat] = $this->target($project_id, $payload);
        $comment = trim((string)($payload['comment'] ?? ''));
        if ($status === 'DEVERIFIED' && $comment === '') {
            throw new ApiException('comment is required to de-verify a value.', 400);
        }
        $uid = $this->uiId($username);
        $this->begin();
        try {
            $t = $this->thread($project_id, $record, $event_id, $field, $instance, $repeat);
            if ($t !== null && $t['query_status'] === 'OPEN') {
                throw new ApiException('Close the open query on this value before changing its verification.', 409);
            }
            if ($t === null) {
                $this->query(
                    'insert into redcap_data_quality_status
                       (non_rule, project_id, record, event_id, field_name, repeat_instrument, instance, query_status)
                     values (1, ?, ?, ?, ?, ?, ?, ?)',
                    [$project_id, $record, $event_id, $field, $repeat, $instance, $status]
                );
                $status_id = $this->lastInsertId();
            } else {
                $status_id = (int)$t['status_id'];
                $this->query('update redcap_data_quality_status set query_status = ? where status_id = ?', [$status, $status_id]);
            }
            $res_id = $this->addResolution($status_id, $uid, 0, null,
                $this->attribute($payload, $comment !== '' ? $comment : 'Verified against source', $project_id), $status);
            $this->commit();
        } catch (\Throwable $e) {
            $this->rollback();
            throw $e;
        }
        $this->audit($project_id, $record, $event_id, $status === 'VERIFIED' ? 'Verify data value' : 'De-verify data value',
            "field = $field, instance = $instance, status_id = $status_id");
        return ['status_id' => $status_id, 'res_id' => $res_id, 'query_status' => $status];
    }


    // ---- form locking --------------------------------------------------------

    const LOCK_COLUMNS = ['ld_id', 'project_id', 'record', 'event_id', 'form_name', 'instance', 'username', 'timestamp'];

    protected function assertLockReady($project_id, $username, $write)
    {
        $have = $this->tableColumns('redcap_locking_data');
        $missing = array_values(array_diff(self::LOCK_COLUMNS, $have));
        if (!empty($missing)) {
            throw new ApiException('Unexpected locking schema; refusing to write. Missing: redcap_locking_data.' . implode(', redcap_locking_data.', $missing), 501);
        }
        if (!$write) {
            return;
        }
        if ($this->rightsLevel($project_id, $username) !== 'superuser' && $this->lockRight($project_id, $username) < 1) {
            throw new ApiException("User '$username' lacks the Record Locking right.", 403);
        }
        $allowed = trim((string)$this->getProjectSetting('allowed-usernames', $project_id));
        if ($allowed !== '' && !in_array($username, array_map('trim', explode(',', $allowed)), true)) {
            throw new ApiException("User '$username' is not allowed to write through this module.", 403);
        }
    }

    protected function lockRight($project_id, $username)
    {
        $row = $this->one(
            'select u.lock_record as own, r.lock_record as role
               from redcap_user_rights u left join redcap_user_roles r on r.role_id = u.role_id
              where u.project_id = ? and u.username = ?',
            [$project_id, $username]
        );
        if ($row === null) {
            return 0;
        }
        return (int)($row['role'] !== null ? $row['role'] : $row['own']);
    }

    protected function formTarget($project_id, $payload)
    {
        $record = $this->requireText($payload, 'record');
        $form = $this->requireText($payload, 'form');
        if ($this->one('select form_name from redcap_metadata where project_id = ? and form_name = ? limit 1', [$project_id, $form]) === null) {
            throw new ApiException("Form '$form' is not in this project.", 400);
        }
        $event = trim((string)($payload['event'] ?? ''));
        $event_id = $event === '' ? $this->firstEventId($project_id) : $this->eventIdFor($project_id, $event);
        if (empty($event_id)) {
            throw new ApiException("Unknown event '$event'.", 400);
        }
        if (!$this->recordExists($project_id, $record)) {
            throw new ApiException("Record '$record' does not exist.", 404);
        }
        return [$record, (int)$event_id, $form, max(1, (int)($payload['instance'] ?? 1))];
    }

    public function lockForm($project_id, $username, $payload)
    {
        [$record, $event_id, $form, $instance] = $this->formTarget($project_id, $payload);
        $existing = $this->one('select ld_id from redcap_locking_data where project_id = ? and record = ? and event_id = ? and form_name = ? and instance = ?',
            [$project_id, $record, $event_id, $form, $instance]);
        if ($existing !== null) {
            return ['locked' => true, 'already_locked' => true, 'ld_id' => (int)$existing['ld_id']];
        }
        $this->query('insert into redcap_locking_data (project_id, record, event_id, form_name, instance, username, timestamp) values (?, ?, ?, ?, ?, ?, ?)',
            [$project_id, $record, $event_id, $form, $instance, $username, $this->now()]);
        $id = $this->lastInsertId();
        $actor = trim((string)($payload['actor'] ?? ''));
        $reason = trim((string)($payload['reason'] ?? ''));
        $this->audit($project_id, $record, $event_id, 'Lock form',
            "form = $form, instance = $instance" . ($actor !== '' ? ", by CDMSTAT user $actor" : '') . ($reason !== '' ? ", reason: $reason" : ''));
        return ['locked' => true, 'already_locked' => false, 'ld_id' => $id];
    }

    public function unlockForm($project_id, $username, $payload)
    {
        [$record, $event_id, $form, $instance] = $this->formTarget($project_id, $payload);
        $reason = $this->requireText($payload, 'reason');
        $q = $this->createQuery();
        $q->add('delete from redcap_locking_data where project_id = ? and record = ? and event_id = ? and form_name = ? and instance = ?',
            [$project_id, $record, $event_id, $form, $instance]);
        $q->execute();
        $n = (int)$q->affected_rows;
        $actor = trim((string)($payload['actor'] ?? ''));
        if ($n > 0) {
            $this->audit($project_id, $record, $event_id, 'Unlock form',
                "form = $form, instance = $instance" . ($actor !== '' ? ", by CDMSTAT user $actor" : '') . ", reason: $reason");
        }
        return ['unlocked' => $n > 0];
    }

    public function listLocks($project_id, $payload)
    {
        $sql = 'select record, event_id, form_name, instance, username, timestamp from redcap_locking_data where project_id = ?';
        $params = [$project_id];
        if (!empty($payload['record'])) {
            $sql .= ' and record = ?';
            $params[] = (string)$payload['record'];
        }
        $res = $this->query($sql . ' order by record, event_id, form_name', $params);
        $out = [];
        while ($r = $res->fetch_assoc()) {
            $r['event'] = $this->uniqueEventName($project_id, (int)$r['event_id']);
            $out[] = $r;
        }
        return ['locks' => $out];
    }

    // ---- helpers -----------------------------------------------------------

    protected function target($project_id, $payload)
    {
        $record = $this->requireText($payload, 'record');
        $field = $this->requireText($payload, 'field');
        if ($this->one('select field_name from redcap_metadata where project_id = ? and field_name = ?', [$project_id, $field]) === null) {
            throw new ApiException("Field '$field' is not in this project.", 400);
        }
        $event = trim((string)($payload['event'] ?? ''));
        $event_id = $event === '' ? $this->firstEventId($project_id) : $this->eventIdFor($project_id, $event);
        if (empty($event_id)) {
            throw new ApiException("Unknown event '$event'.", 400);
        }
        if (!$this->recordExists($project_id, $record)) {
            throw new ApiException("Record '$record' does not exist.", 404);
        }
        $instance = max(1, (int)($payload['instance'] ?? 1));
        $repeat = trim((string)($payload['repeat_instrument'] ?? ''));
        return [$record, (int)$event_id, $field, $instance, $repeat === '' ? null : $repeat];
    }

    protected function thread($project_id, $record, $event_id, $field, $instance, $repeat)
    {
        return $this->one(
            'select status_id, query_status from redcap_data_quality_status
              where project_id = ? and non_rule = 1 and record = ? and event_id = ? and field_name = ? and instance = ?
                and (repeat_instrument = ? or (repeat_instrument is null and ? is null))',
            [$project_id, $record, $event_id, $field, $instance, $repeat, $repeat]
        );
    }

    protected function addResolution($status_id, $uid, $requested, $response, $comment, $current)
    {
        $this->query(
            'insert into redcap_data_quality_resolutions
               (status_id, ts, user_id, response_requested, response, comment, current_query_status)
             values (?, ?, ?, ?, ?, ?, ?)',
            [$status_id, $this->now(), $uid, $requested, $response, $comment, $current]
        );
        return $this->lastInsertId();
    }

    protected function attribute($payload, $comment, $project_id)
    {
        $actor = trim((string)($payload['actor'] ?? ''));
        if ($actor !== '' && $this->getProjectSetting('attribute-actor', $project_id)) {
            return "[CDMSTAT: $actor] $comment";
        }
        return $comment;
    }

    // Status reporting must not fail because an optional feature is missing.
    protected function safely(callable $fn)
    {
        try {
            return $fn();
        } catch (\Throwable $e) {
            return null;
        }
    }

    protected function requireText($payload, $key)
    {
        $v = trim((string)($payload[$key] ?? ''));
        if ($v === '') {
            throw new ApiException("$key is required.", 400);
        }
        return $v;
    }

    protected function one($sql, $params)
    {
        $res = $this->query($sql, $params);
        $row = $res->fetch_assoc();
        return $row ?: null;
    }

    protected function uiId($username)
    {
        $r = $this->one('select ui_id from redcap_user_information where username = ?', [$username]);
        return $r === null ? null : (int)$r['ui_id'];
    }

    // ---- REDCap integration points (overridden in tests) -------------------

    protected function now()
    {
        return date('Y-m-d H:i:s');
    }

    protected function lastInsertId()
    {
        return (int)db_insert_id();
    }

    protected function begin()
    {
        $this->query('start transaction', []);
    }

    protected function commit()
    {
        $this->query('commit', []);
    }

    protected function rollback()
    {
        $this->query('rollback', []);
    }

    protected function firstEventId($project_id)
    {
        return (int)$this->getFirstEventId($project_id);
    }

    protected function eventIdFor($project_id, $unique_event_name)
    {
        return (int)\REDCap::getEventIdFromUniqueEvent($unique_event_name);
    }

    protected function uniqueEventName($project_id, $event_id)
    {
        $names = \REDCap::getEventNames(true, false);
        return is_array($names) && isset($names[$event_id]) ? $names[$event_id] : null;
    }

    protected function recordExists($project_id, $record)
    {
        $table = method_exists('\REDCap', 'getDataTable') ? \REDCap::getDataTable($project_id) : 'redcap_data';
        return $this->one("select 1 as x from $table where project_id = ? and record = ? limit 1", [$project_id, $record]) !== null;
    }

    protected function audit($project_id, $record, $event_id, $description, $changes)
    {
        \REDCap::logEvent("CDMSTAT DRW API: $description", $changes, null, $record, $event_id, $project_id);
    }

    protected function logError(\Throwable $e)
    {
        $this->log('cdmstat_drw error', ['message' => $e->getMessage()]);
    }
}
