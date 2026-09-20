/* Slider de anos do mapa (port do labourvaluesdatapanel, sem i18n nem lista
 * agrupada de indicadores). O servidor apenas informa os limites via
 * mensagem "painel-map-years": o navegador conserva a selecao mais recente
 * e a ajusta a cobertura recebida com snap direcional sobre lacunas. */
(function () {
  'use strict';
  function snapYear(years, value, previous) {
    if (years.includes(value)) return value;
    // Teclado e animacao precisam ultrapassar lacunas, sem voltar sempre
    // ao mesmo ano. Uma nova cobertura, sem direcao, usa o mais proximo.
    if (previous !== undefined && value > previous) return years.find(year => year >= value) ?? years[years.length - 1];
    if (previous !== undefined && value < previous) return years.slice().reverse().find(year => year <= value) ?? years[0];
    return years.reduce((best, year) => Math.abs(year - value) < Math.abs(best - value) ? year : best, years[0]);
  }
  if (typeof module !== 'undefined' && module.exports) module.exports = {snapYear};
  if (typeof document === 'undefined') return;
  const registry = new Map();
  function ensure(sliderId, indicatorId) {
    const key = sliderId + '|' + indicatorId;
    if (registry.has(key)) return registry.get(key);
    const entry = { years: [], lastYear: undefined, sliderId: sliderId };
    const sliderNode = document.getElementById(sliderId);
    if (!sliderNode || !window.jQuery) return null;
    window.jQuery(sliderNode).on('change.painel-map-ano', function () {
      const slider = window.jQuery(sliderNode).data('ionRangeSlider');
      if (!slider || !entry.years.length) return;
      const current = slider.result.from;
      const year = snapYear(entry.years, current, entry.lastYear);
      entry.lastYear = year;
      if (year !== current) {
        slider.update({from: year});
        window.jQuery(sliderNode).trigger('change');
      }
    });
    registry.set(key, entry);
    return entry;
  }
  function initialize() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    window.Shiny.addCustomMessageHandler('painel-map-years', function (message) {
      if (!message || !message.sliderId || !message.indicatorId) return;
      const indicatorNode = document.getElementById(message.indicatorId);
      // Descarta resposta de um indicador que ja foi trocado.
      if (indicatorNode && message.indicator !== undefined) {
        const current = indicatorNode.selectize ? indicatorNode.selectize.getValue() : indicatorNode.value;
        if (current && String(current) !== String(message.indicator)) return;
      }
      const entry = ensure(message.sliderId, message.indicatorId);
      if (!entry) return;
      const values = Array.isArray(message.years) ? message.years : [message.years];
      const years = values.map(Number).filter(Number.isFinite).sort((a, b) => a - b);
      if (!years.length) return;
      const sliderNode = document.getElementById(message.sliderId);
      const slider = window.jQuery(sliderNode).data('ionRangeSlider');
      if (!slider) { entry.years = years; return; }
      const previous = slider.result.from;
      const year = snapYear(years, previous);
      entry.years = years;
      slider.update({min: years[0], max: years[years.length - 1], from: year});
      entry.lastYear = year;
      if (year !== previous) window.jQuery(sliderNode).trigger('change');
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initialize);
  else initialize();
})();
