/* Painel de indicadores do banco de dados do painel: consumidor do núcleo
   do tema (aedi-tema.js, que alterna as paletas gov.br/pb e despacha o
   evento "aedi:paleta"). Aqui ficam só as responsabilidades do painel:
   repassar a paleta ativa ao servidor (cores dos gráficos), restaurar a
   última aba visitada e manter as alturas da topbar/navbar sincronizadas. */
(function () {
  'use strict';

  document.addEventListener('aedi:paleta', function (evento) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('painel_paleta_ativa',
        evento.detail && evento.detail.paleta === 'pb' ? 'pb' : 'govbr');
    }
  });

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
    var aba = evento.target.closest('#painel_nav a[data-value]');
    if (aba) {
      try { localStorage.setItem('painel_aba', aba.dataset.value); } catch (e) { /* ok */ }
    }
  });

  function inicializar() {
    atualizarAlturas();
    var aba = null;
    try { aba = localStorage.getItem('painel_aba'); } catch (e) { /* ok */ }
    if (aba) {
      var link = document.querySelector('#painel_nav a[data-value="' + aba + '"]');
      if (link) link.click();
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', inicializar);
  } else {
    inicializar();
  }
  window.addEventListener('resize', atualizarAlturas);
})();
