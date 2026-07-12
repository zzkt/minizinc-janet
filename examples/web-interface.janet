# Group Meeting Webapp
#
# Usage:
#   janet server.janet          # Start server on port 8000 (default)
#   janet server.janet 8187     # Start server on custom port

(import spork/temple :as temple)

# Config
(var SOLVER-TIMEOUT 120000)
(when-let [env-val (os/getenv "SOLVER_TIMEOUT")]
  (set SOLVER-TIMEOUT (scan-number env-val)))


# Utilities
(defn log [msg]
  (def t (os/date))
  (printf "[%02d:%02d:%02d] %s\n" (t :hours) (t :minutes) (t :seconds) msg)
  (flush))

(defn html-escape [s]
  (->> s
       (string/replace-all "&" "&amp;")
       (string/replace-all "<" "&lt;")
       (string/replace-all ">" "&gt;")))

(defn url-decode [s]
  (->> s
       (string/replace-all "+" " ")
       (string/replace-all "%20" " ")
       (string/replace-all "%2C" ",")
       (string/replace-all "%7C" "|")))

(defn parse-form-body [body]
  (def params @{})
  (when body
    (each pair (string/split "&" body)
      (def parts (string/split "=" pair))
      (when (= (length parts) 2)
        (put params (parts 0) (url-decode (parts 1))))))
  params)

(defn drain [stream]
  (def buf @"")
  (try (while true (buffer/push buf (ev/read stream 1024))) ([_] nil))
  buf)

(defn run-command [cmd]
  (log (string "exec: " cmd))
  (def start (os/clock))
  (def proc (os/spawn ["/bin/sh" "-c" cmd] :p {:out :pipe :err :pipe}))
  (def stdout (string (drain (proc :out))))
  (def stderr (string (drain (proc :err))))
  (os/proc-wait proc)
  (os/proc-close proc)
  (log (string/format "done in %.1fs" (- (os/clock) start)))
  (if (> (length stderr) 0) (string stdout "\n" stderr) stdout))

(defn html-response [body]
  {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body body})


# Shared CSS & JS

(def CSS `<link rel="stylesheet" href="/static/style.css">`)

(def JS `
<script>
function switchTab(btn) {
  btn.closest('.card').querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
  btn.closest('.card').querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
  btn.classList.add('active');
  document.getElementById(btn.dataset.panel).classList.add('active');
}
</script>`)


# Template helpers
(defn status-badge-class [status]
  (def s (if (= (string/slice status 0 1) ":") (string/slice status 1) status))
  (cond
    (string/has-prefix? "optimal" s) "status-optimal"
    (string/has-prefix? "satisf" s)  "status-satisfy"
    :else "status-error"))

(defn capitalize [s]
  (if (> (length s) 0)
    (string (string/ascii-upper (string/slice s 0 1)) (string/slice s 1))
    s))

(defn tab-bar-html [tabs active-tab]
  (string "  <div class=\"section-tabs\">\n"
          (string/join
            (map (fn [t]
                   (string "    <button class=\"tab-btn\""
                           (if (= t active-tab) " active" "")
                           " data-panel=\"panel-" t "\" onclick=\"switchTab(this)\">"
                           (capitalize t) "</button>"))
                 tabs))
          "\n  </div>\n"))

(defn stat-card [val label]
  (string/format "    <div class=\"stat\"><div class=\"stat-val\">%s</div><div class=\"stat-label\">%s</div></div>\n"
                 (string val) label))

(defn pair-bar-html [p]
  (def pct (p :pairs-percent))
  (def bar-class (cond (>= pct 80) "good" (>= pct 50) "ok" :else "low"))
  (string "  <div class=\"pair-coverage\">\n"
          "    <div class=\"pair-detail\"><span>Pairs met: " (string (p :pairs-met)) " / " (string (p :pairs-total))
          "</span><span>" (string pct) "%%</span></div>\n"
          "    <div class=\"pair-bar\"><div class=\"pair-fill " bar-class "\" style=\"width:" (string pct) "%%\"></div></div>\n"
          "    <div class=\"pair-detail\"><span>Exactly once: " (string (p :pairs-once))
          "</span><span>Repeated: " (string (p :pairs-multi)) "</span></div>\n"
          "  </div>\n"))

(defn round-meta-str [round]
  (def n-extras (length (round :extras)))
  (def n-people (reduce + 0 (map length (round :groups))))
  (if (> n-extras 0)
    (string/format "%d groups, %d people, %d extra" (length (round :groups)) n-people n-extras)
    (string/format "%d groups, %d people" (length (round :groups)) n-people)))

(defn summary-header [round]
  (def group-sizes (string/join (map string (map length (round :groups))) ","))
  (def extras (length (round :extras)))
  (if (> extras 0)
    (string/format "Round %d (%s; %d flex)" (round :num) group-sizes extras)
    (string/format "Round %d (%s)" (round :num) group-sizes)))

(defn repeat-rows [people all-rounds person-groups]
  (each person people
    (def assignments (get person-groups person @[]))
    (printf "        <tr><td><strong>%s</strong></td>" (html-escape person))
    (var ridx 0)
    (each round all-rounds
      (def is-extra (find (fn [p] (= p person)) (round :extras)))
      (if is-extra
        (print "<td><span class=\"cell-chip cell-extra\">flex</span></td>")
        (do
          (def assign (if (< ridx (length assignments)) (assignments ridx) "?"))
          (def group-num (scan-number assign))
          (def color-class (if (and group-num (> group-num 0))
                             (string "t" (+ 1 (% (- group-num 1) 6)))
                             "t1"))
          (printf "<td><span class=\"cell-chip %s\">G%s</span></td>"
                  color-class (html-escape assign))))
      (++ ridx))
    (print "</tr>\n")))

(defn extras-row [all-rounds]
  (def has-extras (find (fn [r] (> (length (r :extras)) 0)) all-rounds))
  (when has-extras
    (print "        <tr><td><em style=\"color:#b2bec3\">flexible</em></td>")
    (each round all-rounds
      (def extras (round :extras))
      (if (> (length extras) 0)
        (printf "<td><span class=\"cell-chip cell-extra\">%s</span></td>"
                (html-escape (string/join extras ", ")))
        (print "<td></td>")))
    (print "</tr>\n")))


# Temple templates (compiled once at load time)

(put temple/base-env 'status-badge-class @{:value status-badge-class})
(put temple/base-env 'tab-bar-html @{:value tab-bar-html})
(put temple/base-env 'stat-card @{:value stat-card})
(put temple/base-env 'pair-bar-html @{:value pair-bar-html})
(put temple/base-env 'round-meta-str @{:value round-meta-str})
(put temple/base-env 'summary-header @{:value summary-header})
(put temple/base-env 'repeat-rows @{:value repeat-rows})
(put temple/base-env 'extras-row @{:value extras-row})

  (def index-templ
    (temple/compile
`<!DOCTYPE html>
<html lang="en"><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{{ (args :title) }}</title>
{- (args :css) -}
</head><body>
<div class="container">
  <h1>Group Meeting Solver</h1>
  <p class="subtitle">Assign people to groups across rounds, minimizing repeated pairings</p>
  <div class="card">
    <h2>Example configurations</h2>
    <div class="presets">
      <button class="preset-btn" onclick="applyPreset(this,'9','3,3,3|4,5|9')">Nines (9)</button>
      <button class="preset-btn" onclick="applyPreset(this,'12','3,3,3,3|4,4,4|6,6')">Classroom (12)</button>
      <button class="preset-btn" onclick="applyPreset(this,'15','3x5x7')">Kirkman (15)</button>
      <button class="preset-btn" onclick="applyPreset(this,'9','3x3|3x3|3x3|3x3')">Dagstuhl (9)</button>
    </div>
    <form method="post" action="/solve" onsubmit="showLoading()">
      <div class="form-row">
        <div>
          <label for="people">Number of people</label>
          <input type="number" id="people" name="people" value="9" min="2" max="50">
        </div>
        <div>
          <label for="rounds">Round specifications</label>
          <input type="text" id="rounds" name="rounds" value="3,3,3|4,5|9" placeholder="3,3,3|4,5|9">
        </div>
      </div>
      <p class="help">Group sizes separated by commas, rounds separated by pipes. Use <code>x</code> to repeat: <code>3x5</code> = five groups of 3.</p>
      <br>
      <button type="submit" class="btn" id="solve-btn">Solve</button>
    </form>
  </div>
<script>
function applyPreset(btn,n,r) {
  document.getElementById('people').value = n;
  document.getElementById('rounds').value = r;
  document.querySelectorAll('.preset-btn').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
}
document.querySelector('form').addEventListener('submit', function(e) {
  e.preventDefault();
  var btn = document.getElementById('solve-btn');
  btn.disabled = true;
  btn.innerHTML = '<span class="spinner" style="width:20px;height:20px;border-width:2px;margin:0;vertical-align:middle"></span> Solving...';
  var data = new URLSearchParams(new FormData(this)).toString();
  fetch('/solve', {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body:data})
    .then(function(r){return r.text()})
    .then(function(html){
      document.open();
      document.write(html);
      document.close();
    })
    .catch(function(err){
      btn.disabled = false;
      btn.innerHTML = 'Solve';
      alert('Error: ' + err);
    });
});
</script>
</div>
{- (args :js) -}
</body></html>`))

  (def results-templ
    (temple/compile
`<!DOCTYPE html>
<html lang="en"><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{{ (args :title) }}</title>
{- (args :css) -}
</head><body>
<div class="container">
{% (def p (args :result)) %}
{% (def has-svg (and (args :svg) (> (length (args :svg)) 0))) %}
{% (def tabs (if has-svg @["rounds" "summary" "raw" "svg"] @["rounds" "summary" "raw"])) %}

  <div class="card">
    <div class="results-header">
      <div class="results-title">
        <h1>Meeting Arrangements</h1>
        <div class="results-subtitle">{{ (length (p :rounds)) }} rounds with {{ (p :pairs-total) }} possible pairings</div>
      </div>
      <span class="status-badge {{ (status-badge-class (p :status)) }}">{{ (p :status) }}</span>
    </div>

    <div class="stats">
{% (print (stat-card (length (p :rounds)) "Rounds")) %}
{% (print (stat-card (string/format "%d%%" (p :pairs-percent)) "Coverage")) %}
{% (print (stat-card (string (p :repeats-count)) "Repeats")) %}
{% (print (stat-card (- (p :pairs-total) (p :pairs-met)) "Remaining")) %}
    </div>

{% (when (> (p :pairs-total) 0) (print (pair-bar-html p))) %}
  </div>

  <div class="card">
{% (print (tab-bar-html tabs "rounds")) %}

  <div class="tab-panel active" id="panel-rounds">
{% (each round (p :rounds) (do %}
    <div class="round-card">
      <div class="round-header">
        <span class="round-num">Round {{ (round :num) }}</span>
        <span class="round-meta">{{ (round-meta-str round) }}</span>
      </div>
      <div class="groups-grid">
{% (var tidx 0) %}
{% (each group (round :groups) (do %}
        <div class="group-chip t{{ (+ 1 (% tidx 6)) }}"><span class="tlabel">G{{ (+ tidx 1) }}</span>{{ (string/join group ", ") }}</div>
{% (++ tidx) %}
{% )) %}
{% (when (> (length (round :extras)) 0) (do %}
        <div class="group-chip extra-chip"><span class="extra-label">Flexible</span>{{ (string/join (round :extras) ", ") }}</div>
{% )) %}
      </div>
    </div>
{% )) %}
  </div>

  <div class="tab-panel" id="panel-summary">
{% (when (> (length (p :rounds)) 0) (do %}
{% (def all-rounds (p :rounds)) %}
{% (def person-groups @{}) %}
{% (each line (p :compact) (do %}
{% (def parts (string/split "; " line)) %}
{% (when (>= (length parts) 2) (put person-groups (parts 0) (string/split ", " (parts 1)))) %}
{% )) %}
{% (def all-people @[]) %}
{% (each round all-rounds (do %}
{% (each group (round :groups) (do %}
{% (each person group (do %}
{% (unless (find (fn [pp] (= pp person)) all-people) (array/push all-people person)) %}
{% )) %}
{% )) %}
{% (each person (round :extras) (do %}
{% (unless (find (fn [pp] (= pp person)) all-people) (array/push all-people person)) %}
{% )) %}
{% )) %}
    <div class="text-results">
    <table>
      <thead><tr><th>Person</th>
{% (each round all-rounds (print (string/format "<th>%s</th>" (summary-header round)))) %}
</tr></thead>
      <tbody>
{% (print (repeat-rows all-people all-rounds person-groups)) %}
{% (print (extras-row all-rounds)) %}
      </tbody>
    </table>
    </div>
{% )) %}
  </div>

  <div class="tab-panel" id="panel-raw">
    <div class="raw-output">{{ (args :raw-text) }}</div>
  </div>

{% (when has-svg (do %}
  <div class="tab-panel" id="panel-svg">
    <div class="svg-container" onclick="this.classList.toggle('expanded')">
{% (print (args :svg)) %}
    </div>
  </div>
{% )) %}

  </div>

  <div class="nav-actions">
    <a class="back-link" href="/">
      <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
        <path d="M19 12H5M12 19l-7-7 7-7"/>
      </svg>
      New Arrangement
    </a>
  </div>
</div>
{- (args :js) -}
</body></html>`))

  (def error-templ
    (temple/compile
`<!DOCTYPE html>
<html lang="en"><head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{{ (args :title) }}</title>
{- (args :css) -}
</head><body>
<div class="container">
  <div class="card">
    <h1 style="color:#d63031">Solver Error</h1>
    <pre style="margin-top:12px;background:#f8f9fa;padding:16px;border-radius:8px;font-size:0.85rem;overflow-x:auto">{{ (args :error-text) }}</pre>
    <a class="back-link" href="/">New Arrangement</a>
  </div>
</div>
{- (args :js) -}
</body></html>`))


# Render helpers (capture template output to string)

(defn render-index []
  (string (index-templ :title "Group Meeting Solver" :css CSS :js JS)))

(defn render-results [result raw-text &opt svg-content]
  (string (results-templ :title "Meeting Arrangements" :css CSS :js JS
                         :result result :raw-text raw-text :svg svg-content)))

(defn render-error [error-text]
  (string (error-templ :title "Solver Error" :css CSS :js JS :error-text error-text)))


# Result text parser

(defn parse-result-text [text]
  (def lines (string/split "\n" text))
  (def result @{:lines lines :rounds @[] :status "unknown"
                :extras-count 0 :repeats-count 0
                :pairs-met 0 :pairs-total 0 :pairs-percent 0 :pairs-once 0 :pairs-multi 0
                :repeated-pairs @[] :compact @[]})
  (var in-round -1)
  (var in-compact false)
  (var in-repeated false)

  # Simple prefix → scan-number handlers
  (def simple-fields
    @[[ "Total pairs:" 12 :pairs-total]
      [ "Exactly once:" 13 :pairs-once]
      [ "More than once:" 15 :pairs-multi]])

  (each line lines
    (def t (string/trim line))
    (cond
      (string/has-prefix? "Status:" line)
        (do (def raw (string/trim (string/slice t 7)))
            (put result :status (if (= (string/slice raw 0 1) ":") (string/slice raw 1) raw)))
      (string/has-prefix? "Repeats:" line)
        (put result :repeats-count (or (scan-number t) 0)))
    (each [prefix skip key] simple-fields
      (when (string/has-prefix? prefix t)
        (put result key (or (scan-number (string/trim (string/slice t skip))) 0))))
    (when (string/has-prefix? "Round " t)
      (def num (scan-number (first (string/split " " (string/slice t 6)))))
      (when num
        (++ in-round)
        (array/push (result :rounds) @{:num num :groups @[] :extras @[]})))
    (when (and (>= in-round 0) (string/has-prefix? "Group " t))
      (def parts (string/split ": " t))
      (when (>= (length parts) 2)
        (array/push ((get (result :rounds) in-round) :groups) (string/split ", " (parts 1)))))
    (when (and (>= in-round 0) (string/has-prefix? "Extra:" t))
      (def extra-str (string/slice t 7))
      (when (> (length extra-str) 0)
        (each person (string/split ", " extra-str)
          (array/push ((get (result :rounds) in-round) :extras) person))))
    (when (string/has-prefix? "Met:" t)
      (def parts (string/split " " (string/trim (string/slice t 4))))
      (when (>= (length parts) 1) (put result :pairs-met (or (scan-number (parts 0)) 0)))
      (when (>= (length parts) 2)
        (def raw-pct (parts 1))
        (def pct (string/replace-all "%" ""
                   (string/replace-all ")" ""
                     (string/replace-all "(" "" raw-pct))))
        (put result :pairs-percent (or (scan-number pct) 0))))
    (when (= t "Did not meet:") (set in-repeated true))
    (when (and in-repeated (string/has-prefix? "  " line) (> (length t) 2))
      (array/push (result :repeated-pairs) (string/trim t)))
    (when (and in-repeated (> (length t) 0) (not (string/has-prefix? "  " line))
               (not (= t "Did not meet:")))
      (set in-repeated false))
    (when (string/has-prefix? "Compact" t) (set in-compact true))
    (when (and in-compact (> (length t) 0) (not (string/has-prefix? "Compact" t))
               (string/find ";" t))
      (array/push (result :compact) t)))
  result)


# Request handlers

(defn handle-index []
  (html-response (render-index)))

(defn handle-solve [body]
  (def params (parse-form-body body))
  (def people (scan-number (get params "people" "9")))
  (def rounds-str (get params "rounds" "3,3,3|4,5|9"))
  (log (string/format "solve: %d people, rounds=%s" people rounds-str))

  (def svg-path (string "/tmp/group-solve-" ((os/date) :seconds) ".svg"))
  (def cmd (string/format "janet group-meeting-flex.janet --people %d --rounds '%s' --svg '%s' --time-limit %d 2>&1"
                          people rounds-str svg-path SOLVER-TIMEOUT))
  (def result-text (run-command cmd))

  (def is-error (or (string/has-prefix? "error:" result-text)
                    (string/has-prefix? "compile error" result-text)
                    (string/has-prefix? "TIME LIMIT" result-text)
                    (string/find "No solution found" result-text)))

  (if is-error
    (do
      (log (string "solver error: " (string/slice result-text 0 (min 100 (length result-text)))))
      (html-response (render-error result-text)))
    (do
      (def parsed (parse-result-text result-text))
      (log (string/format "result: status=%s rounds=%d pairs=%d/%d (%d%%) repeats=%d"
                          (parsed :status) (length (parsed :rounds))
                          (parsed :pairs-met) (parsed :pairs-total) (parsed :pairs-percent)
                          (parsed :repeats-count)))
      (def svg-content (try (slurp svg-path) ([_] nil)))
      (try (os/rm svg-path) ([_] nil))
      (html-response (render-results parsed result-text svg-content)))))

(defn handle-static [path]
  (def css-path (string "static" (string/slice path 7)))
  (try
    {:status 200 :headers {"Content-Type" "text/css; charset=utf-8"} :body (slurp css-path)}
    ([_] {:status 404 :headers {"Content-Type" "text/plain"} :body "Not found"})))

(defn handle-request [method path body]
  (cond
    (and (= method "GET")  (= path "/"))     (handle-index)
    (and (= method "POST") (= path "/solve")) (handle-solve body)
    (and (= method "GET")  (string/has-prefix? "/static/" path)) (handle-static path)
    (do (log (string "404: " path))
        {:status 404 :headers {"Content-Type" "text/plain"} :body "Not found"})))


# HTTP layer

(defn read-headers [conn]
  (def buf @"")
  (while true
    (def b (ev/read conn 1))
    (when (= b nil) (break))
    (buffer/push buf b)
    (when (string/has-suffix? "\r\n\r\n" (string buf))
      (break)))
  (string buf))

(defn parse-request [headers-str]
  (def lines (string/split "\r\n" headers-str))
  (def parts (string/split " " (first lines)))
  (var content-length 0)
  (each line lines
    (when (string/has-prefix? "Content-Length:" line)
      (set content-length (scan-number (string/trim (string/slice line 15))))))
  @{:method (parts 0) :path (parts 1) :content-length content-length})

(defn send-response [conn response]
  (def status (get response :status 200))
  (def body (get response :body ""))
  (ev/write conn (string "HTTP/1.1 " status " OK\r\n"))
  (ev/write conn (string "Content-Length: " (length body) "\r\n"))
  (ev/write conn "Connection: close\r\n")
  (eachp [k v] (get response :headers {})
    (ev/write conn (string k ": " v "\r\n")))
  (ev/write conn "\r\n")
  (ev/write conn body)
  (net/close conn))

(defn handle-conn [conn]
  (try
    (do
      (def headers-str (read-headers conn))
      (def req (parse-request headers-str))
      (var body nil)
      (when (> (req :content-length) 0)
        (set body (string (ev/read conn (req :content-length)))))
      (send-response conn (handle-request (req :method) (req :path) body)))
    ([err]
      (log (string "ERROR: " err))
      (try
        (send-response conn {:status 500
                             :headers {"Content-Type" "text/plain"}
                             :body (string "Internal server error: " err)})
        ([_] nil)))))

(defn main [& args]
  (def port (if (> (length args) 1)
              (scan-number ((dyn :args) 1))
              8000))
  (def server (net/listen "127.0.0.1" port))
  (log (string/format "server listening on http://localhost:%d (timeout=%ds)" port (/ SOLVER-TIMEOUT 1000)))
  (forever
    (def conn (net/accept server))
    (when conn
      (ev/spawn (handle-conn conn)))))
