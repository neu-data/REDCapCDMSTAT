<?php
// Self-contained tests for the CDMSTAT DRW module: a stub External Module
// framework backed by an in-memory SQLite database with the REDCap tables the
// module touches. Run with:  php tests/run_tests.php

namespace ExternalModules {
    abstract class AbstractExternalModule
    {
        public $framework;
        public $pdo;
        public $settings = [];
        public $logs = [];

        public function query($sql, $params)
        {
            $st = $this->pdo->prepare($sql);
            $st->execute(array_values($params));
            return new \FakeResult($st);
        }

        public function createQuery()
        {
            return new \FakeQuery($this);
        }

        public function getProjectSetting($key, $pid = null)
        {
            return $this->settings[$key] ?? null;
        }

        public function log($message, $params = [])
        {
            $this->logs[] = [$message, $params];
        }
    }
}

namespace {
    class FakeResult
    {
        private $st;
        public function __construct($st) { $this->st = $st; }
        public function fetch_assoc() { $r = $this->st->fetch(PDO::FETCH_ASSOC); return $r === false ? null : $r; }
    }

    class FakeQuery
    {
        private $m; private $sql = ''; private $params = [];
        public function __construct($m) { $this->m = $m; }
        public function add($sql, $params = []) { $this->sql .= ' ' . $sql; $this->params = array_merge($this->params, (array)$params); return $this; }
        public function addInClause($col, $values)
        {
            $this->sql .= " $col in (" . implode(',', array_fill(0, count($values), '?')) . ')';
            $this->params = array_merge($this->params, $values);
            return $this;
        }
        public function execute() { return $this->m->query($this->sql, $this->params); }
    }

    require __DIR__ . '/../CDMSTATDRW.php';

    class TestModule extends \Neudata\CDMSTATDRW\CDMSTATDRW
    {
        public $audit = [];
        public $events = ['baseline_arm_1' => 41, 'week_12_arm_1' => 42];
        public $dropColumn = null;
        public $clock = 0;

        public function __construct()
        {
            $this->pdo = new PDO('sqlite::memory:');
            $this->pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
            foreach ([
                'create table redcap_projects (project_id int, data_resolution_enabled int)',
                'create table redcap_user_information (ui_id integer primary key, username text, super_user int default 0)',
                'create table redcap_user_rights (project_id int, username text, role_id int, data_quality_resolution int)',
                'create table redcap_user_roles (role_id int, project_id int, data_quality_resolution int)',
                'create table redcap_metadata (project_id int, field_name text)',
                'create table redcap_data (project_id int, event_id int, record text, field_name text, value text)',
                'create table redcap_data_quality_status (status_id integer primary key autoincrement, rule_id int, pd_rule_id int,
                   non_rule int, project_id int, record text, event_id int, field_name text, repeat_instrument text,
                   instance int not null default 1, status int, exclude int not null default 0, query_status text, assigned_user_id int)',
                'create table redcap_data_quality_resolutions (res_id integer primary key autoincrement, status_id int, ts text,
                   user_id int, response_requested int not null default 0, response text, comment text,
                   current_query_status text, upload_doc_id int, field_comment_edited int not null default 0)',
                "insert into redcap_projects values (7, 2), (8, 1)",
                "insert into redcap_user_information (ui_id, username, super_user) values (1, 'svc_cdmstat', 0), (2, 'site_crc', 0), (3, 'viewer', 0), (4, 'admin', 1), (5, 'opener', 0)",
                "insert into redcap_user_rights values (7, 'svc_cdmstat', null, 3), (7, 'site_crc', null, 2), (7, 'viewer', null, 1), (7, 'opener', 9, 0)",
                "insert into redcap_user_roles values (9, 7, 4)",
                "insert into redcap_metadata values (7, 'record_id'), (7, 'weight'), (7, 'sbp')",
                "insert into redcap_data values (7, 41, '1', 'weight', '450'), (7, 42, '1', 'sbp', '300')",
            ] as $sql) {
                $this->pdo->exec($sql);
            }
        }

        protected function now() { $this->clock++; return sprintf('2026-10-26 10:00:%02d', $this->clock); }
        protected function lastInsertId() { return (int)$this->pdo->lastInsertId(); }
        protected function begin() { $this->pdo->beginTransaction(); }
        protected function commit() { $this->pdo->commit(); }
        protected function rollback() { if ($this->pdo->inTransaction()) $this->pdo->rollBack(); }
        protected function firstEventId($pid) { return 41; }
        protected function eventIdFor($pid, $name) { return $this->events[$name] ?? 0; }
        protected function uniqueEventName($pid, $id) { $f = array_flip($this->events); return $f[$id] ?? null; }
        protected function recordExists($pid, $record)
        {
            return $this->one('select 1 as x from redcap_data where project_id = ? and record = ? limit 1', [$pid, $record]) !== null;
        }
        protected function tableColumns($table)
        {
            $cols = [];
            foreach ($this->pdo->query("pragma table_info($table)") as $r) {
                if ($r['name'] !== $this->dropColumn) $cols[] = $r['name'];
            }
            return $cols;
        }
        protected function audit($pid, $record, $event_id, $description, $changes) { $this->audit[] = "$description | $record | $changes"; }

        public function call($action, $payload = [], $user = 'svc_cdmstat', $pid = 7)
        {
            $r = $this->redcap_module_api($action, $payload, $pid, $user, 'json', 'json', ',');
            return [$r['status'], json_decode($r['body'], true)];
        }
    }

    $fails = 0;
    $n = 0;
    function check($cond, $label)
    {
        global $fails, $n;
        $n++;
        if ($cond) { echo "  ok   $label\n"; } else { $fails++; echo "  FAIL $label\n"; }
    }

    echo "drw-status\n";
    $m = new TestModule();
    [$s, $b] = $m->call('drw-status');
    check($s === 200 && $b['drw_enabled'] && $b['schema_ok'] && $b['rights_level'] === 3, 'enabled, schema ok, rights 3');
    check($b['capabilities']['close'] === true, 'service account can close');
    [$s, $b] = $m->call('drw-status', [], 'opener');
    check($b['rights_level'] === 4 && !$b['capabilities']['close'] && $b['capabilities']['open'], 'role rights override user rights');

    echo "guards\n";
    [$s, $b] = $m->call('list-queries', [], 'svc_cdmstat', 8);
    check($s === 403 && strpos($b['error'], 'not enabled') !== false, 'DRW disabled project refused');
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'weight', 'comment' => 'x'], 'viewer');
    check($s === 403, 'view-only user cannot open');
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'nope', 'comment' => 'x']);
    check($s === 400 && strpos($b['error'], 'not in this project') !== false, 'unknown field refused');
    [$s, $b] = $m->call('open-query', ['record' => '999', 'field' => 'weight', 'comment' => 'x']);
    check($s === 404, 'unknown record refused');
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'weight', 'comment' => '']);
    check($s === 400, 'comment required');
    $r = $m->redcap_module_api('list-queries', [], 7, 'svc_cdmstat', 'json', 'xml', ',');
    check($r['status'] === 400, 'non-json returnFormat refused');
    $r = $m->redcap_module_api('list-queries', [], null, null, 'json', 'json', ',');
    check($r['status'] === 401, 'no-auth refused');

    echo "query lifecycle\n";
    $m->settings['attribute-actor'] = true;
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'weight', 'event' => 'baseline_arm_1',
        'comment' => 'Weight 450 kg out of range', 'actor' => 'dana', 'assign_to' => 'site_crc']);
    check($s === 200 && $b['query_status'] === 'OPEN' && $b['already_open'] === false, 'open query');
    $sid = $b['status_id'];
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'weight', 'event' => 'baseline_arm_1', 'comment' => 'again']);
    check($s === 200 && $b['already_open'] === true && $b['status_id'] === $sid, 'opening twice is idempotent');
    [$s, $b] = $m->call('respond', ['status_id' => $sid, 'response' => 'TYPOGRAPHICAL_ERROR', 'comment' => 'Should be 45.0'], 'site_crc');
    check($s === 200 && $b['query_status'] === 'OPEN', 'site responds');
    [$s, $b] = $m->call('respond', ['status_id' => $sid, 'response' => 'BOGUS', 'comment' => 'x'], 'site_crc');
    check($s === 400, 'bad response category refused');
    [$s, $b] = $m->call('close-query', ['status_id' => $sid, 'comment' => 'x'], 'site_crc');
    check($s === 403, 'respond-only user cannot close');
    [$s, $b] = $m->call('close-query', ['status_id' => $sid, 'comment' => 'Corrected in REDCap', 'actor' => 'dana']);
    check($s === 200 && $b['query_status'] === 'CLOSED', 'close');
    [$s, $b] = $m->call('close-query', ['status_id' => $sid, 'comment' => 'again']);
    check($s === 409, 'closing a closed query refused');
    [$s, $b] = $m->call('reopen-query', ['status_id' => $sid, 'comment' => 'Value still wrong']);
    check($s === 200 && $b['query_status'] === 'OPEN', 'reopen');
    [$s, $b] = $m->call('add-comment', ['status_id' => $sid, 'comment' => 'Chasing site']);
    check($s === 200 && $b['query_status'] === 'OPEN', 'comment keeps status');

    echo "verification\n";
    [$s, $b] = $m->call('verify-field', ['record' => '1', 'field' => 'weight', 'event' => 'baseline_arm_1']);
    check($s === 409, 'cannot verify a value with an open query');
    [$s, $b] = $m->call('verify-field', ['record' => '1', 'field' => 'sbp', 'event' => 'week_12_arm_1', 'comment' => 'SDV done']);
    check($s === 200 && $b['query_status'] === 'VERIFIED', 'verify');
    [$s, $b] = $m->call('deverify-field', ['record' => '1', 'field' => 'sbp', 'event' => 'week_12_arm_1']);
    check($s === 400, 'de-verify needs a comment');
    [$s, $b] = $m->call('deverify-field', ['record' => '1', 'field' => 'sbp', 'event' => 'week_12_arm_1', 'comment' => 'Source changed']);
    check($s === 200 && $b['query_status'] === 'DEVERIFIED', 'de-verify');

    echo "reading\n";
    [$s, $b] = $m->call('list-queries', [], 'viewer');
    check($s === 200 && count($b['threads']) === 2, 'viewer can list threads');
    $t = $b['threads'][0];
    check($t['event'] === 'baseline_arm_1' && $t['assigned_to'] === 'site_crc' && count($t['resolutions']) === 5, 'thread detail and resolutions');
    check($t['resolutions'][0]['comment'] === '[CDMSTAT: dana] Weight 450 kg out of range', 'actor attribution on comments');
    check($t['resolutions'][1]['response'] === 'TYPOGRAPHICAL_ERROR' && $t['resolutions'][1]['username'] === 'site_crc', 'response recorded with user');
    $max = $b['max_res_id'];
    [$s, $b] = $m->call('list-queries', ['since_res_id' => $max]);
    check(count($b['threads']) === 0, 'since_res_id filters out seen resolutions');
    [$s, $b] = $m->call('list-queries', ['since_res_id' => $max - 1]);
    check(count($b['threads']) === 1, 'since_res_id returns only threads with new activity');

    echo "settings and schema guard\n";
    $m->settings['allowed-usernames'] = 'someone_else';
    [$s, $b] = $m->call('add-comment', ['status_id' => $sid, 'comment' => 'x']);
    check($s === 403 && strpos($b['error'], 'not allowed') !== false, 'allowed-usernames restricts writes');
    [$s, $b] = $m->call('list-queries');
    check($s === 200, 'allowed-usernames does not restrict reads');
    $m->settings['allowed-usernames'] = '';
    $m->dropColumn = 'current_query_status';
    [$s, $b] = $m->call('add-comment', ['status_id' => $sid, 'comment' => 'x']);
    check($s === 501 && strpos($b['error'], 'current_query_status') !== false, 'unexpected schema refuses writes');
    $m->dropColumn = null;
    [$s, $b] = $m->call('open-query', ['record' => '1', 'field' => 'sbp', 'comment' => 'x'], 'admin');
    check($s === 200, 'superuser bypasses project rights');
    check(count($m->audit) >= 8, 'every write is audited (' . count($m->audit) . ' entries)');

    echo "\n$n checks, $fails failed\n";
    exit($fails ? 1 : 0);
}
