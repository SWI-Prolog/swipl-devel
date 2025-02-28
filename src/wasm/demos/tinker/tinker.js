/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

		 /*******************************
		 *   CONSTANTS AND COMPONENTS   *
		 *******************************/

const user_dir      = "/prolog"
const default_file  = `${user_dir}/scratch.pl`;

const terminal	    = document.getElementById('console');
const output	    = document.getElementById('output');
let   answer;
let   answer_ignore_nl = false;
const input	    = document.getElementById('input');
const more	    = document.getElementById('more');
const trace	    = document.getElementById('trace');
const editor	    = document.getElementById('editor');
const select_file   = document.getElementById('select-file');
const abort	    = document.getElementById('abort');
const keyboard	    = document.getElementById('keyboard');
let   yield	    = null;
let   abort_request = false;
let   history       = { stack: [], current: null };
let   files	    = { current: default_file,
			list: [default_file]
		      };

function user_file(file)
{ return `${user_dir}/${file}`;
}

function is_user_file(file)
{ return file.startsWith(`${user_dir}/`);
}

		 /*******************************
		 *          CODEMIRROR          *
		 *******************************/

let CodeMirror;
let cm;

require.config(
  { paths:
    { "cm/lib/codemirror": "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.9/codemirror.min",
      "cm/addon/edit/matchbrackets": "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.9/addon/edit/matchbrackets.min",
      "cm/mode/prolog": "https://www.swi-prolog.org/download/codemirror/mode/prolog"
    }
  });

function initCodeMirror(cont)
{ function createCM()
  { cm = CodeMirror(document.getElementById("file"),
                    { lineNumbers: true,
		      matchBrackets: true,
		      mode: "prolog",
		      theme: "prolog",
		      prologKeys: true
                    });
  }

  require(["cm/lib/codemirror",
	   "cm/addon/edit/matchbrackets",
	   "cm/mode/prolog/prolog",
	   "cm/mode/prolog/prolog_keys",
	  ], (cm) => {
	    CodeMirror = cm;
	    createCM();
	    restoreFiles();
	    addExamples().then(()=>{cont();});
	  });
}

function loadCss(url)
{ const link = document.createElement("link");
  link.type = "text/css";
  link.rel = "stylesheet";
  link.href = url;
  document.getElementsByTagName("head")[0].appendChild(link);
}

loadCss("https://eu.swi-prolog.org/download/codemirror/theme/prolog.css");

/**
 * Go to a given 1-based line number
 *
 * @param {number} line
 * @param {Object} [options]
 * @param {number} [options.linepos] Go to a specific column
 */

function cm_goto(cm, line, options)
{ options  = options||{};
  const ch = options.linepos||0;

  function clearSearchMarkers(cm)
  { if ( cm._searchMarkers !== undefined )
    { for(let i=0; i<cm._searchMarkers.length; i++)
      cm._searchMarkers[i].clear();
      cm.off("cursorActivity", clearSearchMarkers);
    }
    cm._searchMarkers = [];
  }

  clearSearchMarkers(cm);
  line = line-1;

  cm.setCursor({line:line,ch:ch});
  cm._searchMarkers.push(
    cm.markText({line:line, ch:0},
		{line:line, ch:cm.getLine(line).length},
		{ className:"CodeMirror-search-match",
		  clearOnEnter: true,
		  clearWhenEmpty: true,
		  title: "Target line"
		}));
  cm.on("cursorActivity", clearSearchMarkers);
}


		 /*******************************
		 *    PROLOG OUTPUT STREAMS     *
		 *******************************/

function print_output(line, cls, sgr) {
  if ( line.trim() == "" && answer && answer_ignore_nl )
  { answer_ignore_nl = false;
    return;
  }

  let node;
  if ( sgr && sgr.link )
  { node = document.createElement('a');
    node.href = sgr.link;
    node.target = "_blank";
    node.addEventListener("click", tty_link);
  } else
  { node = document.createElement('span');
    if ( sgr )
    { if ( sgr.color )
      node.style.color = sgr.color;
      if ( sgr.background_color )
	node.background_color.color = sgr.background_color;
      if ( sgr.bold )
	node.classList.add("bold");
      if ( sgr.underline )
	node.classList.add("underline");
    }
  }
  node.classList.add(cls);
  node.textContent = line;
  (answer||output).appendChild(node);
};


function tty_link(ev)
{ const a = ev.target;
  const to = a.href;
  if ( to.startsWith("file://") )
  { ev.preventDefault();
    Prolog.query("tinker:tty_link(Link)", {Link:to}).once();
  }
  // Use default action
}


function getPromiseFromEvent(item, event) {
  return new Prolog.Promise((resolve) => {
    const listener = (ev) => {
      item.removeEventListener(event, listener);
      resolve(ev);
    }
    item.addEventListener(event, listener);
  })
}

async function get_single_char()
{ terminal.classList.add("key");
  keyboard.focus();
  const ev = await getPromiseFromEvent(keyboard, "keyup");
  terminal.classList.remove("key");
  return ev.keyCode;
}

function getCharSize(element)
{ if ( !element.char_size )
  { let temp = document.createElement("span");
    temp.className = "stdout";
    temp.textContent = "test";
    element.appendChild(temp);
    const rect = temp.getBoundingClientRect();
    element.char_size = { h: rect.height,
			  w: rect.width/4
			};
    element.removeChild(temp);
  }
  return element.char_size;
}

function tty_size()
{ const tty = document.querySelector("div.console");
  const wrapper = tty.closest("div.scroll-wrapper");
  const charsz = getCharSize(output);
  return [ Math.floor(wrapper.clientHeight/charsz.h),
	   Math.floor(wrapper.clientWidth/charsz.w)
	 ];
}

		 /*******************************
		 *       OUTPUT STRUCTURE       *
		 *******************************/

/** @return {HTMLDivElement} in which the current query should dump
 * its answer.
 */

function current_answer()
{ return answer;
}


/** Add a structure for a query.  The structure is
 *
 * ```
 * <div class="query-container">
 *   <div class="query-header">?- betweenl(1,3,X).</div
 *   <div class="query-answers">
 *     <div class="query-answer">
 *       <span class="stdout">X = 1;</span>
 *     </div>
 *     <div class="query-answer">
 *       <span class="stdout">X = 2;</span>
 *     </div>
 *     ...
 *   </div>
 * </div>
 * ```
 */

function add_query(query)
{ const div1 = document.createElement("div");
  const div2 = document.createElement("div");
  const div3 = document.createElement("div");
  const div4 = document.createElement("div");
  const btns = document.createElement("span");
  const edit = document.createElement("span");
  const close = document.createElement("span");
  const icon = document.createElement("span");
  btns.className = "query-buttons";
  edit.textContent = "\u270E";
  edit.title = "Copy query to input";
  close.textContent = "\u2715";
  icon.className = "query-collapse";
  icon.title = "Collapse/expand answer";
  btns.appendChild(edit);
  btns.appendChild(icon);
  btns.appendChild(close);

  const prev = last_query();
  if ( prev )
    query_collapsed(prev, true);

  div1.className = "query-container";
  div2.className = "query-header";
  div3.className = "query-answers";
  div4.className = "query-answer";
  div1.appendChild(btns);
  div1.appendChild(div2);
  div1.appendChild(div3);
  div3.appendChild(div4);
  div2.textContent = `?- ${query}`;
  answer = div4;
  edit.addEventListener("click", () => {
    const queryElem = input.querySelector("input");
    queryElem.value = query;
    queryElem.focus();
  });
  close.addEventListener("click", () => div1.remove(), false);
  icon.addEventListener(
    "click",
    (e) => query_collapsed(e.target.closest("div.query-container")));
  output.appendChild(div1);
}

function last_query()
{ const q = output.lastChild;
  if ( q && q.classList.contains("query-container") )
    return q;
  return undefined;
}

function query_collapsed(query, how)
{ if ( how === true )
  query.classList.add("collapsed");
  else if ( how === false )
    query.classList.remove("collapsed");
  else
    query.classList.toggle("collapsed");
}


/** Add a new answer `<div>` after we asked for more answers.
 */
function next_answer()
{ if ( answer )
  { const div4 = document.createElement("div");
    div4.className = "query-answer";
    answer.after(div4);
    answer = div4;
    answer_ignore_nl = true; // suppress the first newline
  }
}

/** Run a query.  Used for e.g., consulting the current file.
 * @param {String} query is the query to run.
 */
function query(query)
{ add_query(query);

  if ( yield && yield.yield == "query" )
  { set_state("run");
    next(yield.resume(query));
  } else
  { Prolog.call(query);
  }
}

		 /*******************************
		 *        ENTER A QUERY         *
		 *******************************/

function submitQuery(queryElem)
{ const input = queryElem.querySelector("input");
  let query = input.value;
  input.value = '';

  if ( queryElem.ex_target == "query" ||
       queryElem.ex_target == "term" )
  { if ( query.trim() == "" )
    { return false;
    } else
    { if ( ! /\.\s*/.test(query) )
        query += ".";
    }
  }

  if ( queryElem.ex_target == "query" )
  { history.stack.push(query);
    history.current = null;
    add_query(query);
  }

  set_state("run");
  next(yield.resume(query));
  return true;
}

function focusInput(queryElem, why)
{ const input  = queryElem.querySelector("input");
  const prompt = queryElem.querySelector("span.prompt");
  switch(why)
  { case "query":
    { prompt.textContent = "?-";
      break;
    }
    default:
    { const s = Prolog.prompt_string(0)||"|: ";
      prompt.textContent = s;
    }
  }
  input.placeholder = `Please enter a ${why}`;
  input.focus();
  queryElem.ex_target = why;
}

input.addEventListener("keydown", (event) =>
{ if ( event.key == "Tab" )
  { const elem   = input.querySelector("input");
    const caret  = elem.selectionStart;
    const all    = elem.value;
    const before = all.slice(0,caret);
    const after  = caret == all.length ? "" : all.slice(caret-all.length);

    function commonPrefix(words)
    { let i = 0;

      while(words[0][i] && words.every(w => w[i] === words[0][i]))
	i++;
      return words[0].slice(0, i);
    }

    function setCompletion(to, del)
    { elem.value = ( before.slice(0, before.length-del.length) +
		     to +
		     after );
    }

    const res = Prolog.query(
      "tinker:complete_input(Before,After,Delete,Completions)",
      {Before:before, After:after}).once();

    if ( res.Completions.length == 1 )
    { setCompletion(res.Completions[0], res.Delete.v);
    } else if ( res.Completions.length > 1 )
    { const common = commonPrefix(res.Completions);
      if ( common.length > 0 )
	setCompletion(common, res.Delete.v);
    }

    event.preventDefault();
  }
});

input.addEventListener("keyup", (event) =>
{ if ( event.defaultPrevented ) return;

  switch(event.key)
  { case "ArrowUp":
      if ( history.current == null )
      {	history.saved = event.target.value;
	history.current = history.stack.length;
      }
      if ( --history.current >= 0 )
      { event.target.value = history.stack[history.current];
      }
      break;
    case "ArrowDown":
      if ( history.current != null )
      { if ( ++history.current < history.stack.length )
	{ event.target.value = history.stack[history.current];
	} else if ( history.current == history.stack.length )
	{ event.target.value = history.saved;
	}
      }
      break;
    case "Enter":
    { submitQuery(input);
      break;
    }
    default:
      return;
  }

  event.preventDefault();
}, true);


		 /*******************************
		 *     CONTROLLING A QUERY      *
		 *******************************/

abort.addEventListener('submit', (e) => {
  e.preventDefault();
  if ( yield && yield.abort )
  { console.log("aborting", yield);
    yield.abort();
  } else
  { console.log("Requesting abort");
    abort_request = true;
  }
}, false);

// Next/Stop

function reply_more(action)
{ if ( yield && yield.yield == "more" )
  { switch(action)
    { case "redo":     print_output(";", "stdout"); next_answer(); break;
      case "continue": print_output(".", "stdout"); answer_ignore_nl = true; break;
    }
    next(yield.resume(action));
  }
}

		 /*******************************
		 *            TRACER            *
		 *******************************/

function reply_trace(action)
{ if ( yield && yield.yield == "trace" )
  { print_output(` [${action}]`, "stderr", {color: "#888"});
    Prolog.call("nl(user_error)", {nodebug:true});

    switch(action)
    { case "goals":
      case "listing":
      case "help":
      { trace_action(action, yield.trace_event);
	break;
      }
      default:
      { set_state("run");
	next(yield.resume(action));
      }
    }
  }
}

function trace_action(action, msg)
{ const prolog = Prolog;

  return prolog.with_frame(() =>
  { const av = prolog.new_term_ref(2);

    prolog.put_chars(av+0, action, prolog.PL_ATOM);
    prolog.bindings.PL_put_term(av+1, msg);
    const flags = prolog.PL_Q_NODEBUG;
    const pred  = prolog.predicate("tinker:trace_action/2");
    const qid   = prolog.bindings.PL_open_query(0, flags, pred, av);
    const rc    = prolog.bindings.PL_next_solution(qid);
    prolog.bindings.PL_close_query(qid);
    return rc;
  });
}

const trace_shortcuts = {
  " ":     "creep",
  "Enter": "creep",
  "a":	   "abort",
  "c":     "creep",
  "g":	   "goals",
  "l":	   "leap",
  "L":	   "listing",
  "r":	   "retry",
  "s":	   "skip",
  "n":     "nodebug",
  "u":	   "up",
  "?":	   "help"
};

trace.addEventListener("keyup", (ev) => {
  if ( ev.defaultPrevented ) return;
  const action = trace_shortcuts[ev.key];
  if ( action )
  { ev.preventDefault();
    ev.stopPropagation();
    reply_trace(action);
  }
});

		 /*******************************
		 *       TOPLEVEL STATES        *
		 *******************************/

function set_state(state)
{ terminal.className = "console " + state;
}

function next(rc)
{ yield = null;

  if ( rc.yield !== undefined )
  { yield = rc;

    Prolog.flush_output();

    if ( abort_request )
    { abort_request = false;
      return next(yield.resume("wasm_abort"));
    }

    switch(rc.yield)
    { case "beat":
        return setTimeout(() => next(yield.resume("true")), 0);
      case "query":
        answer = undefined;
        /*FALLTHROUGH*/
      case "term":
      case "line":
        set_state("prompt "+rc.yield);
        focusInput(input, rc.yield);
        break;
      case "more":
        set_state("more");
        document.getElementById("more.next").focus();
        break;
      case "trace":
      { trace_action("print", yield.trace_event);
        set_state("trace");
        document.getElementById("trace.creep").focus();
        break;
      }
      case "builtin":
        rc.resume((rc)=>next(rc));
        break;
    }
  } else if ( rc.error )
  { if ( rc.message == "Execution Aborted" )
    { Prolog.call("print_message(informational, unwind(abort))");
    } else
    { console.log("Unhandled exception; restarting", rc);
    }
    toplevel();
  }
}

function toplevel()
{ let rc = Prolog.call("wasm_query_loop",
		       { async:true,
			 debugger:true
		       });

  next(rc);
}

		 /*******************************
		 *         START PROLOG         *
		 *******************************/

let Prolog;
let Module;
var options = {
  arguments: ['-g', 'true'],
  locateFile: function(file) { // not needed with swipl-bundle.js
    return '/wasm/' + file;
  },
  on_output: print_output
};

SWIPL(options).then(async (module) =>
    { Module = module;
      Prolog = Module.prolog;
      Module.FS.mkdir(user_dir);
      await Prolog.load_scripts();
      await Prolog.consult("tinker.pl", {module:"system"});
      Prolog.query("tinker:tinker_init(Dir)", {Dir:user_dir}).once();
      Prolog.call("version");
      initCodeMirror(toplevel);
    });

async function addExamples()
{ const json = await fetch("examples/index.json").then((r) =>
  { return r.json();
  });

  if ( Array.isArray(json) && json.length > 0 )
  { const select = select_file;
    const sep = document.createElement("option");
    sep.textContent = "Demos";
    sep.disabled = true;
    select.appendChild(sep);

    json.forEach((ex) =>
      { if ( !hasFileOption(select, user_file(ex.name)) )
	{ const opt = document.createElement("option");
	  opt.className = "url";
	  opt.value = "/wasm/examples/"+ex.name;
	  opt.textContent = (ex.comment||ex.name) + " (demo)";
	  select.appendChild(opt);
	}
      });
  }
}

		 /*******************************
		 *      EDITOR AND CONSULT      *
		 *******************************/

editor.addEventListener('submit', (e) => {
  e.preventDefault();
  saveFile(files.current);
  query(`consult('${files.current}').`);
}, false);

document.getElementById('new-file').onclick = (e) => {
  fname = document.getElementById("file-name");
  e.preventDefault();
  editor.className = "create-file";
  e.target.disabled = true;
  fname.value = "";
  fname.focus();
};

document.getElementById('file-name').onkeydown = (e) => {
  if ( e.key === "Enter" )
  { e.preventDefault();
    document.getElementById('create-button').click();
  }
};

function deleteFile(file)
{ const select = select_file;
  const opt = hasFileOption(select, file);
  let to = opt.nextElementSibling;
  const sep = demoOptionSep(select);
  if ( !to || to == sep )
    to = opt.previousElementSibling;
  if ( !to )
    to = default_file;
  switchToFile(to.value);
  opt.parentNode.removeChild(opt);
  files.list = files.list.filter((n) => (n != file));
  localStorage.removeItem(file);
  Module.FS.unlink(file);
}

document.getElementById('delete-file').onclick = (e) => {
  e.preventDefault();
  const del = selectedFile();

  if ( del == default_file )
  { alert("Cannot delete the default file");
    return;
  }
  if ( !is_user_file(del) )
  { alert("Cannot delete system files");
    return;
  }
  deleteFile(del);
};

function baseName(path)
{ return path.split("/").pop();
}

function hasFileOption(select, name)
{ return Array.from(select.childNodes).find((n) => n.value == name );
}

function demoOptionSep(select)
{ return Array.from(select_file.childNodes).find(
  (n) => n.textContent == "Demos" && n.disabled);
}

function addFileOption(name)
{ const select = select_file;

  if ( !hasFileOption(select, name) )
  { const node = document.createElement('option');
    node.textContent = baseName(name);
    node.value = name;
    node.selected = true;
    const sep = demoOptionSep(select);
    if ( sep )
      select.insertBefore(node, sep);
    else
      select.appendChild(node);
  }
}

function switchToFile(name)
{ let options = Array.from(select_file.childNodes);

  options.forEach((e) => {
    e.selected = e.value == name;
  });

  if ( files.current != name )
  { if ( files.current )
      saveFile(files.current);
    files.current = name;
    if ( !files.list.includes(name) )
      files.list.push(name);
    loadFile(name);
    updateDownload(name);
  }
}

document.getElementById('create-button').onclick = e => {
  e.preventDefault();
  let input = document.getElementById("file-name");
  let name  = input.value.trim();

  if ( /^[a-zA-Z 0-9.-_]+$/.test(name) )
  { if ( ! /\.pl$/.test(name) )
      name += ".pl";

    name = user_file(name);

    addFileOption(name);
    switchToFile(name);

    editor.className = "";
    document.getElementById('new-file').disabled = false;
  } else
  { alert("No or invalid file name!");
  }
};

function selectedFile()
{ opt = select_file.options[select_file.selectedIndex];
  return opt.value;
}

document.getElementById("select-file").onchange = (e) => {
  opt = select_file.options[select_file.selectedIndex];

  if ( opt.className == "url" )
  { fetch(opt.value)
    .then((res) => res.text())
    .then((s) => {
      const name = baseName(opt.value);
      opt.className = "local";
      opt.value = user_file(name);
      opt.textContent = name;
      Module.FS.writeFile(opt.value, s);
      switchToFile(opt.value);
    });
  } else
  { switchToFile(opt.value);
  }
}

		 /*******************************
		 *       UP AND DOWNLOAD        *
		 *******************************/

function updateDownload(name)
{ const btn = document.querySelector("a.btn.download");
  if ( btn )
  { name = baseName(name);
    btn.download = name;
    btn.title = `Download ${name}`;
    btn.href = "download";
  }
}

document.querySelector("a.btn.download").addEventListener("click", (ev) => {
  const text = cm.getValue();
  const data = new Blob([text]);
  const btn = ev.target;
  btn.href = URL.createObjectURL(data);
});

function readAsText(file) {
    return new Promise((resolve, reject) => {
        const fr = new FileReader();
        fr.onerror = reject;
        fr.onload = () => {
            resolve(fr.result);
        }
        fr.readAsText(file);
    });
}

async function download_files(files)
{ for(let i=0; i<files.length; i++)
  { const file = files[i];
    const content = await readAsText(file);
    const name = user_file(baseName(file.name));
    addFileOption(name);
    switchToFile(name);
    cm.setValue(content);
    saveFile(name);
  }
}

document.querySelector("a.btn.upload").addEventListener("click", (ev) => {
  const exch = ev.target.closest("span.exch-files");
  if ( exch.classList.contains("upload-armed") )
  { const files = exch.querySelector('input.upload-file').files;
    download_files(files).then(() => {
      exch.classList.remove("upload-armed");
    });
  } else
  { exch.classList.add("upload-armed")
  }
});

		 /*******************************
		 *        PERSIST FILES         *
		 *******************************/

function persistsFile(name)
{ if ( is_user_file(name) )
  { try
    { let content = Module.FS.readFile(name, { encoding: 'utf8' });
      localStorage.setItem(name, content);
    } catch(e)
    { localStorage.removeItem(name);
    }
  }
}

function restoreFile(name)
{ let content = localStorage.getItem(name)||"";

  if ( content || name == default_file )
  { Module.FS.writeFile(name, content);
    addFileOption(name);
  } else
  { files.list = files.list.filter((n) => (n != name));
  }
}

function restoreFiles()
{ let f = localStorage.getItem("files");
  if ( f ) files = JSON.parse(f);

  files.list.forEach((f) => restoreFile(f));
  if ( !files.list.includes(default_file) )
    files.list.unshift(default_file);

  let current = files.current;
  files.current = null;
  switchToFile(current || default_file);
}

function loadFile(name)
{ name = name || files.current;

  try
  { let content = Module.FS.readFile(name, { encoding: 'utf8' });
    cm.setValue(content);
  } catch(e)
  { cm.setValue("");
  }
}

function saveFile(name, force)
{ if ( force || is_user_file(name) )
  { Module.FS.writeFile(name, cm.getValue());
  }
}

let autosave = true;

window.onunload = (e) =>
{ if ( autosave )
  { localStorage.setItem("history", JSON.stringify(history));
    const l = files.list.filter((n) => is_user_file(n)||n == default_file);
    const save =
	  { list: l,
	    current: l.includes(files.current) ? files.current : default_file
	  };

    localStorage.setItem("files",   JSON.stringify(save));

    save.list.forEach((f) => persistsFile(f));
  }
}

(function restore()
{ let h = localStorage.getItem("history");

  if ( h ) history = JSON.parse(h);
})();

		 /*******************************
		 *          DEMO CALLS          *
		 *******************************/

function add_one(n)
{ return n+1;
}

function promise_any(data)
{ console.log(data);

  return new Promise(function(resolve, reject)
  { resolve(data);
  });
}
