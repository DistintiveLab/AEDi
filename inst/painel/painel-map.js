/* Atualizacao incremental do mapa (port do labourvaluesdatapanel): as
 * geometrias municipais permanecem no Leaflet durante a sessao e, ao mudar
 * indicador/ano/paleta, apenas cores e tooltips atravessam a conexao.
 * O servidor envia a mensagem "painel-map-delta" com o delta por local_id;
 * este gerenciador mantem o estado completo por id (mensagens antigas podem
 * conter menos camadas que a ultima) e repassa a lista integral ao Leaflet. */
(function (root) {
  "use strict";

  const maps = new Map();
  const latest = new Map();
  const pending = new Map();
  const frames = new Set();
  const states = new Map();
  let instance = 0;
  let registered = false;
  const now = () => root.performance ? root.performance.now() : Date.now();
  const nextFrame = root.requestAnimationFrame ?
    root.requestAnimationFrame.bind(root) : (callback) => root.setTimeout(callback, 0);

  function mergeDelta(previous, message) {
    const state = message.reset || !previous ? new Map() : previous;
    (Array.isArray(message.layers) ? message.layers : []).forEach(function (layer) {
      if (layer && layer.id !== undefined) state.set(String(layer.id), layer);
    });
    return state;
  }

  // O gerenciador tambem encontra poligonos de bases temporariamente ocultas.
  // Percorrer map.eachLayer() deixaria essas bases com cores e textos antigos.
  function updateLayers(map, updates) {
    if (!map.layerManager || typeof map.layerManager.getLayer !== "function") {
      return { missing: updates, changed: 0 };
    }
    const missing = [];
    let changed = 0;
    updates.forEach(function (update) {
      const layer = map.layerManager.getLayer("shape", String(update.id));
      if (!layer) {
        missing.push(update);
        return;
      }
      if (layer.options.fillColor !== update.color) {
        layer.setStyle({ fillColor: update.color });
      }
      if (layer._painelLabel !== update.label) {
        if (layer.getTooltip()) {
          layer.setTooltipContent(update.label);
        } else {
          layer.bindTooltip(update.label, { sticky: true, direction: "auto" });
        }
        layer._painelLabel = update.label;
      }
      changed += 1;
    });
    return { missing: missing, changed: changed };
  }

  function schedule(id) {
    if (frames.has(id)) return;
    frames.add(id);
    nextFrame(function () {
      frames.delete(id);
      const state = maps.get(id);
      const updates = pending.get(id);
      if (!state || !state.active || !updates) return;
      const started = now();
      const result = updateLayers(state.map, updates);
      state.stats.updates += 1;
      state.stats.layers = result.changed;
      state.stats.lastDurationMs = now() - started;
      if (result.missing.length) {
        // A mensagem pode chegar antes do addPolygons; layeradd retoma a fila.
        pending.set(id, result.missing);
      } else {
        pending.delete(id);
        state.element.setAttribute("aria-busy", "false");
      }
    });
  }

  function receive(message) {
    if (!message || !message.id || !Array.isArray(message.layers)) return;
    // Durante a animacao do slider de ano, somente o estado mais recente
    // precisa ser desenhado.
    latest.set(message.id, message.layers);
    pending.set(message.id, message.layers);
    const state = maps.get(message.id);
    if (state) state.element.setAttribute("aria-busy", "true");
    schedule(message.id);
  }

  function prepareLegend(state) {
    if (!state.controls) return;
    state.controls.querySelectorAll(".legend").forEach(function (legend) {
      if (legend.dataset.painelLegend) return;
      legend.dataset.painelLegend = "true";
      legend.id = "painel-legenda-mapa";
      const details = root.document.createElement("details");
      details.className = "painel-mapa-legenda";
      details.open = state.legendOpen;
      const summary = root.document.createElement("summary");
      const label = root.document.createElement("span");
      label.textContent = "Legenda";
      summary.append(label);
      const content = root.document.createElement("div");
      content.className = "painel-mapa-legenda-conteudo";
      content.id = state.element.id + "-legend-content";
      summary.setAttribute("aria-controls", content.id);
      while (legend.firstChild) content.appendChild(legend.firstChild);
      const close = root.document.createElement("button");
      close.type = "button";
      close.className = "painel-mapa-legenda-fechar";
      const cross = root.document.createElement("span");
      cross.setAttribute("aria-hidden", "true");
      cross.textContent = "\u00d7";
      const closeLabel = root.document.createElement("span");
      closeLabel.className = "sr-only";
      closeLabel.textContent = "Fechar";
      close.append(cross, closeLabel);
      close.addEventListener("click", function () {
        details.open = false;
        summary.focus();
      });
      content.appendChild(close);
      details.append(summary, content);
      legend.appendChild(details);
      details.addEventListener("toggle", function () {
        if (details.isConnected) state.legendOpen = details.open;
      });
      if (root.L && root.L.DomEvent) {
        root.L.DomEvent.disableClickPropagation(details);
        root.L.DomEvent.disableScrollPropagation(content);
      }
    });
  }

  function prepareTooltip(state, tooltip) {
    if (!tooltip || state.tooltips.has(tooltip) || !root.L || !tooltip._setPosition) return;
    const original = tooltip._setPosition;
    const position = function (point) {
      const node = this.getElement();
      node.style.maxWidth = Math.max(1, Math.min(320, state.element.clientWidth - 16)) + "px";
      node.style.boxSizing = "border-box";
      // Medir o destino, nao o frame anterior da transicao do Leaflet.
      node.style.transitionProperty = "none";
      original.call(this, point);
      const viewport = state.element.getBoundingClientRect();
      const bounds = node.getBoundingClientRect();
      const left = Math.max(viewport.left + 8, Math.min(bounds.left, viewport.right - 8 - bounds.width));
      const top = Math.max(viewport.top + 8, Math.min(bounds.top, viewport.bottom - 8 - bounds.height));
      const dx = left - bounds.left, dy = top - bounds.top;
      node.setAttribute("data-painel-clamped", dx || dy ? "true" : "false");
      if (dx || dy) {
        const current = root.L.DomUtil.getPosition(node);
        root.L.DomUtil.setPosition(node, root.L.point(current.x + dx, current.y + dy));
      }
    };
    state.tooltips.set(tooltip, { original: original, position: position });
    // Somente esta instancia: sticky, setContent e zoom usam o mesmo caminho.
    tooltip._setPosition = position;
    tooltip.update();
  }

  function releaseTooltips(state) {
    state.map.off("tooltipopen", state.onTooltipOpen);
    state.tooltips.forEach(function (record, tooltip) {
      if (tooltip._setPosition === record.position) tooltip._setPosition = record.original;
    });
    state.tooltips.clear();
  }

  function attach(element, map) {
    register();
    const previous = maps.get(element.id);
    if (previous) {
      previous.map.off("layeradd", previous.onLayerAdd);
      previous.map.off("unload", previous.onUnload);
      if (previous.resizeObserver) previous.resizeObserver.disconnect();
      if (previous.legendObserver) previous.legendObserver.disconnect();
      releaseTooltips(previous);
    }
    const state = {
      element: element,
      map: map,
      active: true,
      tooltips: new Map(),
      legendOpen: previous ? previous.legendOpen : false,
      stats: { updates: 0, layers: 0, lastDurationMs: 0 },
      onLayerAdd: function () { if (pending.has(element.id)) schedule(element.id); }
    };
    state.onUnload = function () { state.active = false; releaseTooltips(state); };
    state.onTooltipOpen = function (event) { prepareTooltip(state, event.tooltip); };
    maps.set(element.id, state);
    map.on("layeradd", state.onLayerAdd);
    map.on("unload", state.onUnload);
    map.on("tooltipopen", state.onTooltipOpen);
    if (element.querySelector && root.MutationObserver) {
      state.controls = element.querySelector(".leaflet-control-container");
      if (state.controls) {
        state.legendObserver = new root.MutationObserver(function () { prepareLegend(state); });
        state.legendObserver.observe(state.controls, { childList: true, subtree: true });
        prepareLegend(state);
      }
    }
    if (root.ResizeObserver) {
      state.resizeObserver = new root.ResizeObserver(function () {
        if (element.clientWidth && element.clientHeight) {
          map.invalidateSize({ pan: false, debounceMoveend: true });
        }
      });
      state.resizeObserver.observe(element);
    }
    if (latest.has(element.id)) pending.set(element.id, latest.get(element.id));
    element.setAttribute("aria-busy", "true");
    schedule(element.id);
    root.Shiny.setInputValue(element.id + "_painel_map_ready", ++instance, { priority: "event" });
  }

  const api = { attach: attach, receive: receive, updateLayers: updateLayers,
    stats: function (id) { return maps.has(id) ? maps.get(id).stats : null; } };
  root.PainelMap = api;
  if (typeof module !== "undefined" && module.exports) module.exports = api;

  function register() {
    if (registered || !root.Shiny || !root.Shiny.addCustomMessageHandler) return;
    root.Shiny.addCustomMessageHandler("painel-map-delta", function (message) {
      if (!message || !message.id) return;
      const state = mergeDelta(states.get(message.id), message);
      states.set(message.id, state);
      receive({ id: message.id, layers: Array.from(state.values()) });
    });
    registered = true;
  }
  if (root.Shiny) register();
  else if (root.document) root.document.addEventListener("DOMContentLoaded", register, { once: true });
})(typeof window !== "undefined" ? window : globalThis);
