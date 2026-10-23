/* projectLSA shell behaviour. Everything is delegated, so it keeps working when
   the module content is replaced through renderUI(). */
(function () {
  'use strict';

  // ---- go to a module ---------------------------------------------------
  // The server listens to input$go_<module>; an "event" priority value makes
  // every click register even when the value is identical.
  function go(module) {
    Shiny.setInputValue('go_' + module, Math.random(), { priority: 'event' });
  }
  $(document).on('click keydown', '[data-go]', function (e) {
    if (e.type === 'keydown' && e.key !== 'Enter' && e.key !== ' ') return;
    e.preventDefault();
    go($(this).attr('data-go'));
    $('.pl-app').removeClass('panel-open');
  });

  // ---- module changed (message from app.R) ------------------------------
  Shiny.addCustomMessageHandler('pl_module', function (m) {
    var app = $('#pl-app');
    app.attr('data-module', m.id).removeClass('ctl-off');
    $('.pl-nav a[data-go]').removeClass('active').removeAttr('aria-current');
    var a = $('.pl-nav a[data-go="' + m.id + '"]').addClass('active').attr('aria-current', 'page');
    $('#pl-chip-text').text(m.title);
    $('.pl-modlist a').removeClass('active').filter('[data-go="' + m.id + '"]').addClass('active');
    $('.pl-modmenu').removeClass('open'); $('#pl-modbtn').attr('aria-expanded', 'false');
    $('#pl-stage-title').text(m.title);
    $('#pl-stage-sub').text(m.sub || '');
    var st = document.querySelector('.pl-stage'); if (st) st.scrollTop = 0;
    // while the method is being opened, say so (not that a model is being fitted)
    var wm = document.getElementById('pl-working-msg');
    if (wm && m.id !== 'home') { wm.textContent = 'Opening ' + (m.title || 'the method'); wm.setAttribute('data-fixed', '1'); window.plOpening = true; }
  });
  $(document).on('shiny:idle', function () {
    if (window.plOpening) { window.plOpening = false; var wm = document.getElementById('pl-working-msg'); if (wm) wm.setAttribute('data-fixed', '0'); }
  });

  // ---- views of the active module (sidebar sub-navigation) --------------
  // Views flagged data-noctl do not use the module's control panel, so it steps aside there
  function applyCtl(btn) {
    $('#pl-app').toggleClass('ctl-off', btn && btn.attr('data-noctl') === '1').toggleClass('needs-model', !!btn && btn.attr('data-needsmodel') === '1');
    $('.pl-tabgroup').each(function () {
      var a = $(this).find('button[data-tab].active');
      $(this).toggleClass('has-active', a.length > 0);
      $(this).find('.pl-tg-cur').text(a.length ? ' \u00b7 ' + (a.attr('data-title') || '') : '');
    });
  }
  function tabLink(value) {
    return $('.pl-stage .tabbable > ul.nav > li > a[data-value="' + value + '"]').first();
  }
  $(document).on('click', '.pl-subnav button[data-tab]', function () {
    var link = tabLink($(this).attr('data-tab'));
    if (link.length) link.tab('show');
    $('#pl-app').removeClass('panel-open');
  });
  // Stay in sync when the server switches tabs with updateTabsetPanel()
  $(document).on('shown.bs.tab', '.pl-stage a[data-toggle="tab"]', function (e) {
    var value = $(e.target).attr('data-value');
    var btn = $('.pl-subnav button[data-tab="' + value + '"]');
    if (!btn.length) return;                         // nested tab sets have no entry
    $('.pl-subnav button').removeClass('active').removeAttr('aria-current');
    btn.addClass('active').attr('aria-current', 'page');
    applyCtl(btn);
    $('#pl-stage-title').text(btn.attr('data-title') || '');
    $('#pl-stage-sub').text(btn.attr('data-sub') || '');
    var st = document.querySelector('.pl-stage'); if (st) st.scrollTop = 0;
  });
  // After a module renders, mark the view that is showing (its first one). A tab that is
  // already showing fires no event, so this is done here. It runs when Shiny goes idle and
  // whenever the list of views itself changes, so it cannot be missed.
  function syncViews() {
    var sub = $('.pl-subnav button[data-tab]');
    if (!sub.length || sub.filter('.active').length) return;
    var cur = $('.pl-stage .tabbable > ul.nav > li.active > a').first().attr('data-value');
    var btn = cur ? sub.filter('[data-tab="' + cur + '"]') : sub.first();
    if (!btn.length) btn = sub.first();
    btn = btn.first().addClass('active').attr('aria-current', 'page');
    applyCtl(btn);
    $('#pl-stage-title').text(btn.attr('data-title') || '');
    $('#pl-stage-sub').text(btn.attr('data-sub') || '');
    var link = $('.pl-stage .tabbable > ul.nav > li > a[data-value="' + btn.attr('data-tab') + '"]').first();
    if (link.length && !link.parent().hasClass('active')) link.tab('show');
  }
  $(document).on('shiny:idle', syncViews);
  $(function () {
    var box = document.getElementById('pl-subnav');
    if (box) new MutationObserver(function () { setTimeout(syncViews, 30); }).observe(box, { childList: true, subtree: true });
    var stage = document.getElementById('pl_stage');
    if (stage) new MutationObserver(function () { setTimeout(syncViews, 30); }).observe(stage, { childList: true });
  });

  // ---- dropdown groups in the top row ----
  function closeGroups() { $('.pl-tabgroup.open').removeClass('open').find('.pl-tabgroup-btn').attr('aria-expanded', 'false'); }
  $(document).on('click', '.pl-tabgroup-btn', function (e) {
    e.stopPropagation();
    var g = $(this).closest('.pl-tabgroup'), was = g.hasClass('open');
    closeGroups();
    if (!was) {
      var r = this.getBoundingClientRect();
      g.addClass('open'); $(this).attr('aria-expanded', 'true');
      g.find('.pl-tabmenu').css({ left: Math.round(Math.min(r.left, window.innerWidth - 240)), top: Math.round(r.bottom + 2) });
    }
  });
  $(document).on('click', function (e) { if (!$(e.target).closest('.pl-tabgroup').length) closeGroups(); });
  $(document).on('click', '.pl-tabmenu button[data-tab]', closeGroups);
  $(document).on('keydown', function (e) { if (e.key === 'Escape') closeGroups(); });
  $(function () { $('.pl-stage, .pl-tabsrow').on('scroll', closeGroups); });

  // ---- persistent control panel is optional -----------------------------
  $(document).on('shiny:value', function (e) {
    if (e.name !== 'pl_controls') return;
    var v = e.value, has = false;
    if (v && typeof v === 'object') has = !!(v.html && String(v.html).replace(/\s+/g, '').length);
    else if (typeof v === 'string') has = v.replace(/\s+/g, '').length > 0;
    $('#pl-app').toggleClass('has-ctl', has);
  });
  // Wide screens: hide / show the panel so the workspace can use the full width.
  // Small screens: the panel slides over the workspace instead.
  $(function () { try { if (localStorage.getItem('pl-ctl-hidden') === '1') $('#pl-app').addClass('ctl-hidden'); } catch (e) {} });
  $(document).on('click', '.pl-controls-toggle', function () {
    var app = $('#pl-app');
    if (window.innerWidth <= 980) { app.toggleClass('panel-open'); return; }
    var hidden = app.toggleClass('ctl-hidden').hasClass('ctl-hidden');
    try { localStorage.setItem('pl-ctl-hidden', hidden ? '1' : '0'); } catch (e) {}
    setTimeout(function () { $(window).trigger('resize'); }, 280);   // let plots re-measure
  });

  // ---- options sidebar of a view: fold it away for a full-width workspace --
  $(document).on('click', '.pl-side-toggle', function () {
    $(this).closest('.pl-split').toggleClass('side-collapsed');
    setTimeout(function () { $(window).trigger('resize'); }, 260);   // let plots re-measure
  });

  // ---- sidebar: labelled list, or a slim icon rail when folded ------------
  // The choice is remembered; folding gives the workspace the full width.
  // this script is loaded in <head>, so anything that touches the page waits for the DOM
  // folded to the icon rail by default; once the user expands it, that choice is remembered
  $(function () {
    var v = null; try { v = localStorage.getItem('pl-collapsed'); } catch (e) {}
    if (v !== '0') $('#pl-app').addClass('collapsed');
  });
  $(document).on('click', '#pl-collapse', function () {
    var c = $('#pl-app').toggleClass('collapsed').hasClass('collapsed');
    try { localStorage.setItem('pl-collapsed', c ? '1' : '0'); } catch (e) {}
    setTimeout(function () { $(window).trigger('resize'); }, 280);   // let plots re-measure
  });

  // ---- dropdown lists are drawn on <body>, so no panel can clip or cover them --------------
  // (selectize puts them inside the input's container by default, where overflow:hidden/auto
  // panels hid the options, e.g. the colour scheme of the path diagram)
  (function () {
    function setDefault() { if (window.Selectize && Selectize.defaults) Selectize.defaults.dropdownParent = 'body'; }
    function flip() {                       // open upward when there is more room above
      if (!window.Selectize || Selectize.prototype.__plFlip) return;
      var orig = Selectize.prototype.positionDropdown;
      Selectize.prototype.positionDropdown = function () {
        orig.apply(this, arguments);
        var $c = this.$control, $d = this.$dropdown, r = $c[0].getBoundingClientRect(), h = window.innerHeight;
        var below = h - r.bottom - 12, above = r.top - 12, need = Math.min($d.outerHeight(), 270);
        var content = $d.find('.selectize-dropdown-content');
        if (below < need && above > below) {
          var room = Math.min(above, 270);
          content.css('max-height', Math.max(room - 8, 120) + 'px');
          $d.css({ top: $c.offset().top - $d.outerHeight() - 2 });
        } else content.css('max-height', Math.max(Math.min(below - 8, 260), 120) + 'px');
      };
      Selectize.prototype.__plFlip = true;
    }
    setDefault(); flip(); $(function () { setDefault(); flip(); }); $(document).on('shiny:connected', setDefault);
  })();

  // ---- decimal separator: one setting for the whole app, remembered in this browser -------
  function setDec(d, fromUser) {
    $('.pl-dec button').each(function () { $(this).attr('aria-pressed', $(this).attr('data-dec') === d ? 'true' : 'false'); });
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('dec_sep', d, { priority: 'event' });
    if (fromUser) { try { localStorage.setItem('pl-dec', d); } catch (e) {} }
  }
  $(document).on('click', '.pl-dec button', function () { setDec($(this).attr('data-dec'), true); });
  $(document).on('shiny:connected', function () {
    var d = '.'; try { d = localStorage.getItem('pl-dec') || '.'; } catch (e) {}
    setDec(d === ',' ? ',' : '.', false);
  });

  // ---- figures in colour or in black and white (journals often ask for the latter) -----------
  function setFig(m, fromUser) {
    $('.pl-fig button').each(function () { $(this).attr('aria-pressed', $(this).attr('data-fig') === m ? 'true' : 'false'); });
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('fig_mode', m, { priority: 'event' });
    if (fromUser) { try { localStorage.setItem('pl-fig', m); } catch (e) {} }
  }
  $(document).on('click', '.pl-fig button', function () { setFig($(this).attr('data-fig'), true); });
  $(document).on('shiny:connected', function () {
    var m = 'color'; try { m = localStorage.getItem('pl-fig') || 'color'; } catch (e) {}
    setFig(m === 'mono' ? 'mono' : 'color', false);
  });

  // ---- AI settings drawer (button at the top right) ----------------------
  window.plDrawer = function (id, open) {
    var d = document.getElementById(id);
    if (!d) return;
    if (open) $('.pl-drawer.open').not(d).each(function () { this.classList.remove('open'); this.setAttribute('aria-hidden', 'true'); });   // one drawer at a time
    d.classList.toggle('open', !!open);
    d.setAttribute('aria-hidden', open ? 'false' : 'true');
    if (open) setTimeout(function () { var f = d.querySelector('select, input'); if (f) f.blur(); }, 0);
  };
  window.plAiDrawer = function (open) { window.plDrawer('pl-ai-drawer', open); };
  $(document).on('keydown', function (e) { if (e.key === 'Escape') $('.pl-drawer.open').each(function () { window.plDrawer(this.id, false); }); });

  // one event per click, handled by serverAISettings.R
  window.plAiAct = function (id, action) {
    Shiny.setInputValue('ai_slot_action', { id: id, action: action, nonce: Math.random() }, { priority: 'event' });
  };

  // status dot on the button: none / ready / cooling
  Shiny.addCustomMessageHandler('pl_ai_status', function (m) {
    $('#pl-ai-btn .pl-dot').attr('data-state', m.state);
    $('#pl-ai-btn').attr('title', m.text).attr('aria-label', m.text);
  });

  // ---- one working indicator: the project logo card at the bottom --------------------------
  // Shiny's own progress notifications and the "please wait" dialogs are hidden by the style sheet;
  // their text is shown on this card instead, so there is a single place to look. When nothing
  // names the step, the card rotates through generic messages.
  var workMsgs = ['Just a moment', 'Getting things ready', 'Almost there'];   // generic only: specific steps come from the progress text
  var workIdx = 0;
  function workSource() {
    var t = '';
    var pm = document.querySelector('.shiny-progress-notification .progress-message, .shiny-progress .progress-text .progress-message');
    if (pm) { t = pm.textContent.trim(); var pd = document.querySelector('.shiny-progress-notification .progress-detail, .shiny-progress .progress-detail'); if (pd && pd.textContent.trim()) t += ' ' + pd.textContent.trim(); }
    if (!t) { var rf = document.querySelector('#shiny-notification-rf_note .shiny-notification-content-text'); if (rf) t = rf.textContent.trim(); }
    if (!t) { var m = document.querySelector('.modal-content:not(:has(.modal-header)):not(:has(.modal-footer)) .modal-body'); if (m) t = m.textContent.trim(); }
    return t.replace(/\s+/g, ' ').slice(0, 90);
  }
  function workSync() {
    var el = document.getElementById('pl-working-msg'); if (!el) return;
    if ($('#pl-working').hasClass('show-toast')) return;                  // a finished/warning message is on show
    var t = workSource();
    if (window.plOpening && !t) return;                                   // keep "Opening ..." until the method is ready
    if (t && el.textContent !== t) el.textContent = t;
    el.setAttribute('data-fixed', t ? '1' : '0');
  }
  // Messages the app would show as pop-ups ("LTA finished", warnings, errors) use the same card,
  // so there is one compact place for everything the app says about what it is doing.
  var toastTimer = null, toastSeen = new WeakSet();
  function showToast(kind, text) {
    var card = document.getElementById('pl-working'); if (!card) return;
    var title = { message: 'Done', warning: 'Please check', error: 'Something went wrong' }[kind] || 'Done';
    card.setAttribute('data-state', kind === 'error' ? 'error' : kind === 'warning' ? 'warn' : 'done');
    document.getElementById('pl-working-title').textContent = title;
    var el = document.getElementById('pl-working-msg'); el.textContent = text; el.setAttribute('data-fixed', '1');
    card.classList.add('show-toast');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(hideToast, kind === 'error' ? 12000 : kind === 'warning' ? 8000 : 4500);
  }
  function hideToast() {
    var card = document.getElementById('pl-working'); if (!card) return;
    card.classList.remove('show-toast'); card.setAttribute('data-state', 'busy');
    document.getElementById('pl-working-title').textContent = 'Working on it';
    document.getElementById('pl-working-msg').setAttribute('data-fixed', '0');
  }
  $(document).on('click', '#pl-working', function () { if ($(this).hasClass('show-toast')) { clearTimeout(toastTimer); hideToast(); } });
  function readToast(n) {
    if (n.id === 'shiny-notification-rf_note' || n.classList.contains('shiny-progress-notification')) return;   // progress text goes to the busy card
    var t = n.querySelector('.shiny-notification-content-text, .shiny-notification-content');
    var txt = t ? t.textContent.replace(/\s+/g, ' ').trim() : '';
    if (!txt) return;
    var kind = n.classList.contains('shiny-notification-error') ? 'error' : n.classList.contains('shiny-notification-warning') ? 'warning' : 'message';
    showToast(kind, txt.slice(0, 160));
  }
  function scanToasts() {
    document.querySelectorAll('#shiny-notification-panel .shiny-notification').forEach(function (n) {
      if (toastSeen.has(n)) return; toastSeen.add(n);
      setTimeout(function () { readToast(n); }, 80);             // the content is added just after the box
    });
  }
  $(function () {
    new MutationObserver(function () { workSync(); scanToasts(); }).observe(document.body, { childList: true, subtree: true, characterData: true });
  });
  setInterval(function () {
    if (!document.documentElement.classList.contains('shiny-busy')) { workIdx = 0; return; }
    var el = document.getElementById('pl-working-msg');
    if (!el || el.getAttribute('data-fixed') === '1' || window.plOpening || $('#pl-working').hasClass('show-toast')) return;
    workIdx = (workIdx + 1) % workMsgs.length;
    el.textContent = workMsgs[workIdx];
  }, 2600);

  window.plAiSave = function (id) {
    var card = $('.pl-slot[data-id="' + id + '"]');
    var v = { id: id, nonce: Math.random() };
    card.find('[data-f]').each(function () { v[$(this).attr('data-f')] = $(this).val(); });
    Shiny.setInputValue('ai_slot_edit', v, { priority: 'event' });
  };

  // ---- label beside a hovered icon (only while the sidebar is folded) -----
  var tip = $();
  $(function () { tip = $('<div id="pl-tip" role="tooltip"></div>').appendTo('body'); });
  function tipText(el) {
    var t = el.getAttribute('data-tip') || el.getAttribute('data-title');
    if (!t) {                                            // links keep their name in title=""
      t = el.getAttribute('title');
      if (t) { el.setAttribute('data-tip', t); el.removeAttribute('title'); }   // no double tooltip
    }
    if (!t) { var s = el.querySelector('span'); t = s ? s.textContent : ''; }
    return (t || '').trim();
  }
  function showTip(el) {
    if (!$('#pl-app').hasClass('collapsed')) return;
    var t = tipText(el);
    if (!t) return;
    var r = el.getBoundingClientRect();
    tip.text(t).css({ left: Math.round(r.right + 12), top: Math.round(r.top + r.height / 2) }).addClass('show');
  }
  function hideTip() { tip.removeClass('show'); }
  var TIP_ITEMS = '.pl-nav a, .pl-subnav button, .pl-brand';
  $(document).on('mouseenter focusin', TIP_ITEMS, function () { showTip(this); });
  $(document).on('mouseleave focusout click', TIP_ITEMS, hideTip);
  $(document).on('click', '#pl-collapse', hideTip);

  // ---- method switcher in the top bar -----------------------------------
  $(document).on('click', '#pl-modbtn', function (e) {
    e.stopPropagation();
    if ($('#pl-app').attr('data-module') === 'home') return;
    var open = $('.pl-modmenu').toggleClass('open').hasClass('open');
    $(this).attr('aria-expanded', open ? 'true' : 'false');
  });
  $(document).on('click', function (e) {
    if (!$(e.target).closest('.pl-modmenu').length) { $('.pl-modmenu').removeClass('open'); $('#pl-modbtn').attr('aria-expanded', 'false'); }
  });
  $(document).on('keydown', function (e) { if (e.key === 'Escape') $('.pl-modmenu').removeClass('open'); });


  // ---- the guide for the open tab, inside the AI assistant ----------------------------------
  // One floating element only: when the open method has results, the assistant's header says a
  // guide is ready for this tab (a small pulsing dot). Opening the assistant lands on its Guide
  // tab, where "Explain this tab" asks the AI for what to look at and what it means. Without a model
  // the guide asks for an API key instead.
  var coach = { ready: false, key: '', tab: '', title: '', mode: 'idle', seen: {} };   // mode: idle | busy | result
  function coachTab() {
    var b = $('.pl-subnav button.active').first();
    return { tab: b.attr('data-tab') || '', title: (b.attr('data-title') || b.text() || '').trim() };
  }
  function coachSkip(tab) { return !tab || /report|about|setting/i.test(tab); }
  function guideTab() { return $('#ai_widget_tabs a').filter(function () { return /guide/i.test($(this).text()); }).first(); }
  function coachRender() {
    var t = coachTab(), key = (document.getElementById('pl-app').getAttribute('data-module') || '') + ':' + t.tab;
    if (key !== coach.key) { coach.key = key; if (coach.mode !== 'busy') coach.mode = 'idle'; }
    coach.tab = t.tab; coach.title = t.title;
    var offer = coach.ready && !coachSkip(t.tab);
    var hdr = $('#ai_widget_header');
    hdr.toggleClass('has-guide', offer && !coach.seen[key]);
    hdr.find('.pl-ai-title small').text(offer ? 'A guide is ready for this tab' : 'Ask about your results');
    if (coach.mode !== 'idle') return;                                  // keep an answer or a pending request
    if (!offer) {
      $('#pl-coach-msg').html('<b>Tab guide</b><br>Run an analysis and I will explain what to look at in each tab and what it means.');
      $('#pl-coach-actions').empty();
      return;
    }
    $('#pl-coach-msg').html('<b>Want a quick guide to this tab?</b><br>I can explain what to look at in &ldquo;' + $('<i>').text(t.title).html() + '&rdquo; and what it means.');
    $('#pl-coach-actions').html('<button type="button" class="pl-coach-go" data-coach="ask"><i class="fa-solid fa-wand-magic-sparkles"></i> Explain this tab</button>');
  }
  Shiny.addCustomMessageHandler('pl_coach', function (m) { coach.ready = !!m.ready; if (!coach.ready) coach.mode = 'idle'; coachRender(); });
  $(document).on('shown.bs.tab click', '.pl-subnav button, .pl-stage a[data-toggle="tab"]', function () { setTimeout(coachRender, 150); });
  $(document).on('shiny:idle', coachRender);
  // opening the assistant while a guide is waiting lands on the Guide tab
  $(document).on('click', '#ai_widget_header, #toggle_ai_widget', function () {
    setTimeout(function () {
      if ($('#ai_widget_body_wrapper').is(':visible') && $('#ai_widget_header').hasClass('has-guide')) { guideTab().tab('show'); coach.seen[coach.key] = true; coachRender(); }
    }, 60);
  });
  $(document).on('click', '[data-coach="ask"]', function () {
    coach.mode = 'busy'; coach.seen[coach.key] = true;
    $('#pl-coach-msg').html('<b>Reading your results&hellip;</b><br>This takes a few seconds.');
    $('#pl-coach-actions').empty();
    Shiny.setInputValue('pl_coach_ask', { tab: coach.tab, title: coach.title, nonce: Math.random() }, { priority: 'event' });
  });
  $(document).on('click', '[data-coach="settings"]', function () { window.plAiDrawer(true); });
  $(document).on('click', '[data-coach="reset"]', function () { coach.mode = 'idle'; coachRender(); });
  Shiny.addCustomMessageHandler('pl_coach_answer', function (m) {
    coach.mode = 'result';
    if (m.state === 'nokey') {
      $('#pl-coach-msg').html('<b>I need an API key to explain this in detail.</b><br>Add a model in the AI settings (free-tier providers are enough), then press Explain again.');
      $('#pl-coach-actions').html('<button type="button" class="pl-coach-go" data-coach="settings"><i class="fa-solid fa-key"></i> Open AI settings</button><button type="button" class="pl-coach-later" data-coach="ask">Explain again</button>');
    } else if (m.state === 'error') {
      $('#pl-coach-msg').html('<b>I could not get an answer.</b><br>' + m.html);
      $('#pl-coach-actions').html('<button type="button" class="pl-coach-go" data-coach="ask">Try again</button><button type="button" class="pl-coach-later" data-coach="settings">AI settings</button>');
    } else {
      $('#pl-coach-msg').html('<div class="pl-coach-answer">' + m.html + '</div><div class="pl-coach-via">' + $('<i>').text(m.via || '').html() + '</div>');
      $('#pl-coach-actions').html('<button type="button" class="pl-coach-later" data-coach="reset">Done</button>');
    }
    guideTab().tab('show');
  });

  // ---- result tables: Excel export plus "copy for Word" in APA style ----------------------------
  // The CSV button is hidden by the style sheet. "Copy for Word" puts the whole table on the clipboard
  // as HTML with APA 7 table formatting (rules only above, below and under the header; Times New
  // Roman; no leading zero for values that cannot exceed 1), so pasting into Word gives a finished table.
  // the highlighting of a cell (fill, text colour, bold) as it is shown: set inline by the tables or by a few classes
  function cellCss(td) {
    var css = '', st = td.style, $td = $(td);
    var bg = st.backgroundColor, col = st.color, fw = st.fontWeight;
    if ($td.hasClass('is-max')) { bg = bg || '#d9f1ee'; col = col || '#065f58'; fw = fw || 'bold'; }
    if ($td.hasClass('is-flag')) { bg = bg || '#fbe0e3'; col = col || '#8f1d2a'; fw = fw || 'bold'; }
    if (bg && !/^(transparent|inherit|initial|white|#fff|#ffffff|rgba?\(255,\s*255,\s*255(,\s*1)?\)|rgba\(0,\s*0,\s*0,\s*0\))$/i.test(bg.trim())) css += 'background-color:' + bg + ';';
    if (col) css += 'color:' + col + ';';
    if (fw && (fw === 'bold' || parseInt(fw, 10) >= 600)) css += 'font-weight:bold;';
    return css;
  }
  function aptTableHtml(headers, rows, dec, styles) {
    var ncol = headers.length, numCol = [], bounded = [];
    for (var j = 0; j < ncol; j++) {
      var vals = rows.map(function (r) { return r[j]; }).filter(function (x) { return x !== ''; });
      var nums = vals.map(function (x) { return /^-?\d*\.?\d+$/.test(x) ? parseFloat(x) : NaN; });
      numCol[j] = vals.length > 0 && nums.every(function (x) { return !isNaN(x); });
      bounded[j] = numCol[j] && nums.every(function (x) { return Math.abs(x) <= 1; }) && vals.some(function (x) { return /\.\d{2,}/.test(x); });
    }
    function fmt(x, j) {
      if (!numCol[j]) return x;
      if (bounded[j]) x = x.replace(/^(-?)0\./, '$1.');
      return dec === ',' ? x.replace('.', ',') : x;
    }
    function esc(t) { return $('<div>').text(t).html(); }
    var cell = 'padding:2pt 7pt;font-family:\'Times New Roman\',Times,serif;font-size:11pt;background-color:#ffffff;color:#000000;';
    var h = '<table bgcolor="#ffffff" style="border-collapse:collapse;background-color:#ffffff;font-family:\'Times New Roman\',Times,serif;font-size:11pt;"><thead><tr>';
    headers.forEach(function (t, j) { h += '<th style="' + cell + 'border-top:1.5pt solid #000;border-bottom:1pt solid #000;font-weight:normal;font-style:italic;text-align:' + (j === 0 ? 'left' : 'center') + ';">' + esc(t) + '</th>'; });
    h += '</tr></thead><tbody>';
    rows.forEach(function (r, i) {
      h += '<tr>';
      r.forEach(function (x, j) { h += '<td style="' + cell + (i === rows.length - 1 ? 'border-bottom:1.5pt solid #000;' : '') + 'text-align:' + (j === 0 ? 'left' : 'center') + ';' + ((styles && styles[i] && styles[i][j]) || '') + '">' + esc(fmt(x, j)) + '</td>'; });
      h += '</tr>';
    });
    return h + '</tbody></table>';
  }
  function copyHtml(html, plain, done) {
    function legacy() {
      var d = document.createElement('div'); d.contentEditable = 'true'; d.style.cssText = 'position:fixed;left:-9999px;top:0;';
      d.innerHTML = html; document.body.appendChild(d);
      var r = document.createRange(); r.selectNodeContents(d); var sel = window.getSelection(); sel.removeAllRanges(); sel.addRange(r);
      var ok = false; try { ok = document.execCommand('copy'); } catch (e) {}
      sel.removeAllRanges(); document.body.removeChild(d); done(ok);
    }
    if (navigator.clipboard && window.ClipboardItem) {
      navigator.clipboard.write([new ClipboardItem({ 'text/html': new Blob([html], { type: 'text/html' }), 'text/plain': new Blob([plain], { type: 'text/plain' }) })]).then(function () { done(true); }, legacy);
    } else legacy();
  }
  window.plCopyHtml = copyHtml;
  function copyApaTable(btn) {
    var wrap = $(btn).closest('.dataTables_wrapper'), tbl = wrap.find('table.dataTable').last();
    if (!tbl.length || !$.fn.dataTable.isDataTable(tbl[0])) return;
    var api = new $.fn.dataTable.Api(tbl[0]), st = api.settings()[0];
    var len = api.page.len(), page = api.page(), all = !st.oFeatures.bServerSide;
    if (all) api.page.len(-1).draw(false);                       // every row, so each one is formatted
    var headers = api.columns().header().toArray().map(function (h) { return $(h).text().replace(/\s+/g, ' ').trim(); });
    var rows = [], styles = [];
    api.rows({ search: 'applied', order: 'applied' }).nodes().toArray().forEach(function (tr) {
      rows.push($(tr).children('td').toArray().map(function (td) { return $(td).text().replace(/\s+/g, ' ').trim(); }));
      styles.push($(tr).children('td').toArray().map(cellCss));
    });
    if (all) { api.page.len(len).draw(false); api.page(page).draw(false); }
    var dec = $('.pl-dec button[aria-pressed="true"]').attr('data-dec') || '.';
    var plain = [headers.join('\t')].concat(rows.map(function (r) { return r.join('\t'); })).join('\n');
    copyHtml(aptTableHtml(headers, rows, dec, styles), plain, function (ok) {
      showToast(ok ? 'message' : 'warning', ok ? 'Table copied with its highlighting. Paste it into Word.' : 'Could not copy the table. Use Export Excel instead.');
    });
  }
  function addCopyButton(settings, tries) {
    var box = $(settings.nTableWrapper).find('.dt-buttons').first();
    if (!box.length) { if (tries < 20) setTimeout(function () { addCopyButton(settings, tries + 1); }, 60); return; }   // the Buttons extension builds its bar just after init
    if (box.find('.pl-copy-apa').length) return;
    $('<button type="button" class="dt-button pl-copy-apa" title="Copy the table to paste into Word, in APA style"><span><i class="fa-regular fa-copy"></i> Copy for Word</span></button>').appendTo(box);
  }
  $(document).on('init.dt', function (e, settings) { addCopyButton(settings, 0); });
  // tables that are re-rendered later get their button too
  function ensureCopyButtons() {
    $('.dataTables_wrapper .dt-buttons').each(function () {
      if (!$(this).find('.pl-copy-apa').length)
        $('<button type="button" class="dt-button pl-copy-apa" title="Copy the table to paste into Word, in APA style"><span><i class="fa-regular fa-copy"></i> Copy for Word</span></button>').appendTo(this);
    });
  }
  $(document).on('shiny:value shiny:idle', function () { setTimeout(ensureCopyButtons, 120); });
  $(document).on('click', '.pl-copy-apa', function (e) { e.preventDefault(); copyApaTable(this); });

  // ---- tables drawn while their tab was hidden are finished when the tab is shown ----------------
  function fixTables() {
    try {
      if (window.HTMLWidgets && HTMLWidgets.staticRender) {
        $('.pl-stage .datatables.html-widget').filter(function () { return !$(this).find('table').length; }).length && HTMLWidgets.staticRender();
      }
      if ($.fn.dataTable) $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
    } catch (e) {}
  }
  $(document).on('shown.bs.tab shiny:idle', function () { setTimeout(fixTables, 120); setTimeout(fixTables, 700); });

  // ---- every plot can be copied as an image (paste into Word) ----------------------------------
  function svgToPngBlob(svg, cb) {
    var box = svg.getBoundingClientRect(), w = Math.max(1, Math.round(box.width)), h = Math.max(1, Math.round(box.height));
    var clone = svg.cloneNode(true); clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg'); clone.setAttribute('width', w); clone.setAttribute('height', h);
    var img = new Image();
    img.onload = function () {
      var c = document.createElement('canvas'); c.width = w * 3; c.height = h * 3;
      var g = c.getContext('2d'); g.fillStyle = '#fff'; g.fillRect(0, 0, c.width, c.height); g.drawImage(img, 0, 0, c.width, c.height);
      c.toBlob(cb, 'image/png');
    };
    img.onerror = function () { cb(null); };
    img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(new XMLSerializer().serializeToString(clone));
  }
  function plotBlob(box, cb) {
    var gd = box.querySelector('.js-plotly-plot');
    if (gd && window.Plotly) { Plotly.toImage(gd, { format: 'png', scale: 2 }).then(function (u) { return fetch(u); }).then(function (r) { return r.blob(); }).then(cb, function () { cb(null); }); return; }
    var img = box.querySelector('img');
    if (img && img.src) { fetch(img.src).then(function (r) { return r.blob(); }).then(cb, function () { cb(null); }); return; }
    var svg = box.querySelector('svg');
    if (svg) { svgToPngBlob(svg, cb); return; }
    cb(null);
  }
  $(document).on('click', '.pl-plot-copy', function (e) {
    e.preventDefault(); e.stopPropagation();
    var box = this.parentNode;
    plotBlob(box, function (blob) {
      if (!blob) { showToast('warning', 'There is no image to copy yet.'); return; }
      if (navigator.clipboard && window.ClipboardItem) {
        navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]).then(
          function () { showToast('message', 'Image copied. Paste it into Word.'); },
          function () { showToast('warning', 'The browser blocked copying. Right-click the plot and choose Copy image.'); });
      } else showToast('warning', 'This browser cannot copy images. Right-click the plot and choose Copy image.');
    });
  });
  function ensurePlotButtons() {
    $('.pl-stage .shiny-plot-output, .pl-stage .html-widget.girafe, .pl-stage .html-widget.plotly').each(function () {
      if ($(this).children('.pl-plot-copy').length) return;
      $('<button type="button" class="pl-plot-copy" title="Copy the image to paste into Word"><i class="fa-regular fa-copy"></i> Copy image</button>').appendTo(this);
    });
  }

  // ---- the decimal separator applies to what is already on screen ------------------------------
  // Plots redraw on the server (see plot_helpers.R); tables are converted here, cell by cell, and
  // converted back when the point is chosen again.
  var NUM = /^([-\u2212+]?\d+)\.(\d+)$/;
  function applyDec() {
    var d = $('.pl-dec button[aria-pressed="true"]').attr('data-dec') || '.';
    $('.pl-stage td, .pl-stage th, .pl-stat-item b, .pl-stat-big b, .pl-chip-kv b, .pl-stage .pl-q3 td').each(function () {
      if (this.children.length) return;
      var t = this.textContent, dot = this.getAttribute('data-pldot');
      if (d === ',') { if (NUM.test(t.trim())) { this.setAttribute('data-pldot', t); this.textContent = t.replace('.', ','); } }
      else if (dot !== null) { if (t === dot.replace('.', ',')) this.textContent = dot; this.removeAttribute('data-pldot'); }
    });
  }
  var tickTimer = null;
  function tick() { clearTimeout(tickTimer); tickTimer = setTimeout(function () { ensurePlotButtons(); applyDec(); }, 90); }
  $(document).on('shiny:value shiny:idle draw.dt shown.bs.tab', tick);
  $(document).on('click', '.pl-dec button', function () { setTimeout(applyDec, 60); setTimeout(applyDec, 700); });
  $(function () { new MutationObserver(tick).observe(document.body, { childList: true, subtree: true }); });

  // ---- table notes fold away; column headers explain themselves on hover -----------------------
  function foldNotes() {
    $('.pl-stage .badge-info:not([data-folded]), .pl-stage .pl-stat-note:not([data-folded])').each(function () {
      var el = $(this); el.attr('data-folded', '1');
      var det = $('<details class="pl-note"><summary><i class="fa-solid fa-circle-info"></i> Notes</summary><div class="pl-note-body"></div></details>');
      det.insertBefore(el); det.find('.pl-note-body').append(el);
    });
  }
  var HINTS = {
    'aic': 'Akaike information criterion. Lower is better.', 'bic': 'Bayesian information criterion. Lower is better; it penalises complexity more than AIC.',
    'sabic': 'Sample-size adjusted BIC. Lower is better.', 'hq': 'Hannan-Quinn criterion. Lower is better.', 'loglik': 'Log-likelihood of the model. Higher (closer to zero) is better.',
    '\u03c7\u00b2': 'Chi-square test statistic.', 'df': 'Degrees of freedom.', 'p(\u03c7\u00b2)': 'p value of the chi-square test. p < .05 means the more complex model fits significantly better.',
    'n_itemfit': 'Number of items with acceptable item fit (p > .05). Higher is better.', 'n_itemld': 'Number of item pairs with local dependence (|Q3| > .20). Lower is better.',
    'a': 'Discrimination: how sharply the item separates respondents. Higher means more discriminating.', 'b': 'Difficulty: the position of the item on the trait scale.',
    'g': 'Lower asymptote (guessing).', 'u': 'Upper asymptote.', '\u03c7\u00b2_itemfit': 'Item-fit statistic of the chosen type (for example S-X2).',
    'p(\u03c7\u00b2)_itemfit': 'p value of the item fit. p < .05 flags a misfitting item.', 'rmsea': 'Root mean square error of approximation. Below .05 is good, below .08 acceptable.',
    'h\u00b2': 'Communality: the share of the item variance explained by the factors.', 'msa': 'Measure of sampling adequacy (KMO) of the item. .50 is the minimum.',
    'entropy': 'Classification quality. Closer to 1 means clearer separation; .80 or higher is good.', 'av_prob': 'Average posterior probability of belonging to the assigned class.',
    'prob_min': 'Smallest average posterior probability among the classes.', 'prob_max': 'Largest average posterior probability among the classes.',
    'n_min': 'Share of the sample in the smallest class.', 'n_max': 'Share of the sample in the largest class.', 'blrt_p': 'Bootstrap likelihood ratio test p value. p < .05 favours k classes over k-1.',
    'gsq': 'Likelihood-ratio statistic G2 for absolute model fit.', 'p_gsq': 'Bootstrap p value of G2. p >= .05 means acceptable absolute fit.', 'deviance': 'Deviance comparing the K-class model with the K+1 model.',
    'p_deviance': 'Bootstrap p value of the deviance test. p < .05 means adding a class improves fit.', 'n_class': 'Number of classes in the model.', 'n_profiles': 'Number of profiles in the model.',
    'model': 'Model specification (for LPA, the variance-covariance structure of the profiles).', 'estimate': 'Estimated parameter value.', 'std.estimate': 'Standardized estimate.', 'se': 'Standard error of the estimate.',
    's.e.': 'Standard error of the estimate.', 'p-value': 'Probability of a result this extreme if the true value were zero.', 'z': 'Estimate divided by its standard error.', 'cfi': 'Comparative fit index. .95 or higher is good, .90 acceptable.',
    'tli': 'Tucker-Lewis index. .95 or higher is good, .90 acceptable.', 'srmr': 'Standardized root mean square residual. Below .08 is acceptable.', 'ave': 'Average variance extracted. .50 or higher supports convergent validity.',
    'cr': 'Composite reliability. .70 or higher is acceptable.', 'composite reliability': 'Composite reliability. .70 or higher is acceptable.', 'unstd. loading': 'Factor loading on the raw scale of the items.',
    'std. loading': 'Standardized factor loading.', 'residual variance': 'Variance of the item not explained by the factor.', 'factor 1': 'Loading of each item on this factor.', 'theta': 'Level of the latent trait.',
    'information': 'Test information: precision of measurement at this trait level.', 'proportion var': 'Share of the total variance explained by the factor.', 'cumulative var': 'Variance explained by this and the earlier factors.',
    'ss loadings': 'Sum of squared loadings: the variance the factor accounts for.', 'category': 'Descriptive label of the value (Kaiser).', 'item': 'Item (variable) name.'
  };
  function hintTables() {
    $('.pl-stage table th:not([data-hint])').each(function () {
      var t = $(this).text().replace(/\s+/g, ' ').trim().toLowerCase();
      var h = HINTS[t] || HINTS[t.replace(/\s+/g, '')];
      $(this).attr('data-hint', h || '');
    });
  }
  var hintEl = $();
  $(function () { hintEl = $('<div id="pl-hint" role="tooltip"></div>').appendTo('body'); });
  var hintTimer = null;
  $(document).on('mouseenter', '.pl-stage th[data-hint]', function () {
    var h = this.getAttribute('data-hint'); if (!h) return;
    var el = this; clearTimeout(hintTimer);
    hintTimer = setTimeout(function () {
      var r = el.getBoundingClientRect();
      hintEl.text(h).css({ left: Math.max(8, Math.min(window.innerWidth - 270, r.left + r.width / 2 - 130)), top: Math.round(r.bottom + 6) }).addClass('show');
    }, 160);
  });
  $(document).on('mouseleave click', '.pl-stage th[data-hint]', function () { clearTimeout(hintTimer); hintEl.removeClass('show'); });
  $(document).on('scroll', function () { hintEl.removeClass('show'); });
  $(document).on('shiny:value shiny:idle draw.dt shown.bs.tab', function () { setTimeout(function () { foldNotes(); hintTables(); }, 120); });
  $(function () { new MutationObserver(function () { setTimeout(function () { foldNotes(); hintTables(); }, 150); }).observe(document.body, { childList: true, subtree: true }); });

  // ---- every result table can be copied for Word, not only the ones with an export bar ----------
  // One floating button follows the table under the pointer. Tables that already carry
  // "Copy for Word" in their export bar are skipped.
  var tcopy = $(), tcopyTarget = null, tcopyTimer = null;
  $(function () {
    tcopy = $('<button type="button" id="pl-tcopy" title="Copy the table to paste into Word, in APA style"><i class="fa-regular fa-copy"></i> Copy for Word</button>').appendTo('body');
  });
  function tableHostFor(t) {
    var $t = $(t);
    if ($t.closest('.dataTables_scrollHead').length) return null;
    var wrap = $t.closest('.dataTables_wrapper');
    if (wrap.length) return wrap.find('.pl-copy-apa').length ? null : wrap[0];
    if ($t.find('tr').length < 2) return null;
    return t;
  }
  function placeTcopy(host) {
    var r = host.getBoundingClientRect();
    if (r.width < 80 || r.height < 24) return;
    tcopyTarget = host;
    tcopy.css({ left: Math.round(Math.min(window.innerWidth - 130, r.right - 118)), top: Math.round(Math.max(52, r.top - 1)) }).addClass('show');
  }
  $(document).on('mouseover', '.pl-stage table', function () {
    var host = tableHostFor(this); if (!host) return;
    clearTimeout(tcopyTimer); placeTcopy(host);
  });
  $(document).on('mouseout', '.pl-stage table', function () { tcopyTimer = setTimeout(function () { tcopy.removeClass('show'); }, 350); });
  $(document).on('mouseenter', '#pl-tcopy', function () { clearTimeout(tcopyTimer); });
  $(document).on('mouseleave', '#pl-tcopy', function () { tcopy.removeClass('show'); });
  $(document).on('scroll wheel', function () { tcopy.removeClass('show'); });
  function domTableData(t) {
    var $t = $(t), headers = [], rows = [], styles = [];
    var $hr = $t.find('thead tr').last();
    if (!$hr.length) $hr = $t.find('tr').first();
    $hr.children('th,td').each(function () { headers.push($(this).text().replace(/\s+/g, ' ').trim()); });
    var $body = $t.find('tbody tr'); if (!$t.find('thead').length) $body = $t.find('tr').slice(1);
    $body.each(function () {
      var r = [], st = []; $(this).children('td,th').each(function () { r.push($(this).text().replace(/\s+/g, ' ').trim()); st.push(cellCss(this)); });
      if (r.some(function (x) { return x !== ''; })) { rows.push(r); styles.push(st); }
    });
    var n = headers.length; rows = rows.map(function (r) { while (r.length < n) r.push(''); return r.slice(0, Math.max(n, r.length)); });
    return { headers: headers, rows: rows, styles: styles };
  }
  $(document).on('click', '#pl-tcopy', function (e) {
    e.preventDefault(); if (!tcopyTarget) return;
    if ($(tcopyTarget).is('.dataTables_wrapper')) { copyApaTable(tcopyTarget); return; }
    var d = domTableData(tcopyTarget), dec = $('.pl-dec button[aria-pressed="true"]').attr('data-dec') || '.';
    // a decimal comma already on screen is turned back before formatting, then applied again
    var rows = d.rows.map(function (r) { return r.map(function (x) { return dec === ',' && /^[-\u2212+]?\d+,\d+$/.test(x) ? x.replace(',', '.') : x; }); });
    var plain = [d.headers.join('\t')].concat(rows.map(function (r) { return r.join('\t'); })).join('\n');
    copyHtml(aptTableHtml(d.headers, rows, dec, d.styles), plain, function (ok) {
      showToast(ok ? 'message' : 'warning', ok ? 'Table copied with its highlighting. Paste it into Word.' : 'Could not copy the table.');
    });
  });
})();
