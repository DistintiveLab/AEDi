/* Painel de indicadores do DW AEDi: paleta govbr/pb persistente, ultima aba
   visitada e sincronizacao da paleta com o servidor (cores dos graficos). */
(function () {
  'use strict';
  var atual = 'govbr';

  function aplicar(paleta) {
    document.body.classList.toggle('painel-pb', paleta === 'pb');
    try { localStorage.setItem('painel_paleta', paleta); } catch (e) { /* ok */ }
    var botao = document.getElementById('painel_paleta_btn');
    if (botao) {
      botao.textContent = paleta === 'pb' ? 'Cores Gov.br' : 'Preto e branco';
      botao.setAttribute('aria-pressed', paleta === 'pb' ? 'false' : 'true');
      botao.title = paleta === 'pb'
        ? 'Mudar para a paleta Gov.br (azul)'
        : 'Mudar para preto e branco com roxo Distintive';
    }
  }

  function informarServidor() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('painel_paleta_ativa', atual);
    }
  }

  function inicial() {
    var salva = null;
    try { salva = localStorage.getItem('painel_paleta'); } catch (e) { /* ok */ }
    if (salva === 'govbr' || salva === 'pb') return salva;
    var raiz = document.getElementById('painel_raiz');
    var param = raiz ? raiz.getAttribute('data-paleta') : null;
    return param === 'pb' ? 'pb' : 'govbr';
  }

  function atualizarAlturas() {
    var topbar = document.querySelector('.painel-topbar');
    var nav = document.querySelector('.navbar');
    var topo = topbar ? topbar.offsetHeight : 56;
    document.documentElement.style.setProperty('--p-topbar-h', topo + 'px');
    if (nav) {
      document.documentElement.style.setProperty('--p-nav-h', (topo + nav.offsetHeight) + 'px');
    }
  }

  document.addEventListener('click', function (evento) {
    var botao = evento.target.closest('#painel_paleta_btn');
    if (botao) {
      atual = atual === 'pb' ? 'govbr' : 'pb';
      aplicar(atual);
      informarServidor();
      return;
    }
    var aba = evento.target.closest('#painel_nav a[data-value]');
    if (aba) {
      try { localStorage.setItem('painel_aba', aba.dataset.value); } catch (e) { /* ok */ }
    }
  });

  function inicializar() {
    atual = inicial();
    aplicar(atual);
    informarServidor();
    atualizarAlturas();
    var aba = null;
    try { aba = localStorage.getItem('painel_aba'); } catch (e) { /* ok */ }
    if (aba) {
      var link = document.querySelector('#painel_nav a[data-value="' + aba + '"]');
      if (link) link.click();
    }
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('painel-paleta', function (mensagem) {
      atual = mensagem && mensagem.paleta === 'pb' ? 'pb' : 'govbr';
      aplicar(atual);
      informarServidor();
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', inicializar);
  } else {
    inicializar();
  }
  window.addEventListener('resize', atualizarAlturas);
})();
