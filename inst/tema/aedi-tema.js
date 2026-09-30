/* Núcleo de tema do AEDi: alterna as paletas gov.br e preto-e-branco
   Distintive (classe aedi-pb no <body>), persiste a escolha em
   localStorage (chave "aedi_paleta"; sem escolha salva vale o
   data-paleta da div #aedi_tema_raiz) e atualiza rotulo/aria de todos
   os botões .aedi-tema-btn. A cada aplicação despacha o evento
   "aedi:paleta" no document — consumidores (ex.: o painel, que recore
   gráficos no servidor) escutam o evento em vez de duplicar o toggle. */
(function () {
  'use strict';
  var atual = 'govbr';

  function aplicar(paleta) {
    atual = paleta === 'pb' ? 'pb' : 'govbr';
    document.body.classList.toggle('aedi-pb', atual === 'pb');
    try { localStorage.setItem('aedi_paleta', atual); } catch (e) { /* ok */ }
    var botoes = document.querySelectorAll('.aedi-tema-btn');
    for (var i = 0; i < botoes.length; i++) {
      botoes[i].textContent = atual === 'pb' ? 'Cores Gov.br' : 'Preto e branco';
      botoes[i].setAttribute('aria-pressed', atual === 'pb' ? 'false' : 'true');
      botoes[i].title = atual === 'pb'
        ? 'Mudar para a paleta Gov.br (azul)'
        : 'Mudar para preto e branco com roxo Distintive';
    }
    document.dispatchEvent(
      new CustomEvent('aedi:paleta', { detail: { paleta: atual } }));
  }

  function inicial() {
    var salva = null;
    try { salva = localStorage.getItem('aedi_paleta'); } catch (e) { /* ok */ }
    if (salva === 'govbr' || salva === 'pb') return salva;
    var raiz = document.getElementById('aedi_tema_raiz');
    var param = raiz ? raiz.getAttribute('data-paleta') : null;
    return param === 'pb' ? 'pb' : 'govbr';
  }

  document.addEventListener('click', function (evento) {
    var botao = evento.target.closest
      ? evento.target.closest('.aedi-tema-btn')
      : null;
    if (botao) {
      evento.preventDefault();
      aplicar(atual === 'pb' ? 'govbr' : 'pb');
    }
  });

  function inicializar() { aplicar(inicial()); }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', inicializar);
  } else {
    inicializar();
  }
})();
