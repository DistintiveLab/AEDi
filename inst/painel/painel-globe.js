/* Globo de localidades (port do wlv-country-globe.js do labourvaluesdatapanel).
 * Globo ortografico do mundo com as delimitacoes territoriais do nivel
 * corrente do banco de dados do painel: arrastar gira livremente pelo
 * mundo, a roda do mouse e os botoes aproximam e afastam (estilo Google
 * Earth) e clicar numa area com dados atualiza a aba Regiao via
 * setInputValue. No nivel municipal a base sao as UFs com o municipio
 * escolhido destacado, a UF inteira em foco e a malha de bordas dos
 * demais municipios do estado (a mensagem traz o geojson do destaque, do
 * contexto e da malha). Requer painel-geo.js. A geometria do nivel
 * chega pela mensagem Shiny "painel-globe" (string GeoJSON do banco de
 * dados); o contorno mundial vem do asset local painel-mundo.geojson por
 * fetch (o globo tambem funciona sem ele). Cores acompanham a paleta do
 * painel (govbr/pb) via variaveis CSS. */
(function (root) {
  "use strict";

  const document = root.document;
  const geo = () => root.PainelGeo;
  const clamp = (value, min, max) => Math.max(min, Math.min(max, value));
  // Rotacao livre pelo mundo inteiro (lambda completo, phi ate os polos) e
  // zoom aproximado/afastado estilo Google Earth: 1 e o globo inteiro e o
  // maximo deixa uma area municipal preencher boa parte do disco.
  const LAMBDA = [-180, 180];
  const PHI = [-90, 90];
  const ZOOM = [1, 256];
  // Uma primeira mensagem ausente e distinta de uma lista de UFs vazia.
  let latest = null;
  let controller = null;
  let registered = false;
  let bootFrame = 0;
  let handshake = 0;

  const labels = {
    globe: "Globo interativo das delimitações territoriais do IBGE",
    instruction: "Arraste para girar, aproxime com a roda ou pelos botões e clique em uma área com dados para escolher a localidade.",
    loading: "Carregando o globo…",
    error: "Não foi possível carregar o globo. Você pode escolher a localidade na aba Região.",
    retry: "Tentar novamente",
    unavailable: "Sem dados neste indicador",
    available: "Com dados",
    selected: "Localidade selecionada",
    north: "Girar para o norte", south: "Girar para o sul",
    west: "Girar para o oeste", east: "Girar para o leste",
    reset: "Centralizar seleção",
    zoomIn: "Aproximar", zoomOut: "Afastar",
    ocean: "Oceano",
    empty: "Nenhuma área com dados neste indicador."
  };

  function cssVar(name, fallback) {
    if (!document.body) return fallback;
    const value = root.getComputedStyle(document.body).getPropertyValue(name).trim();
    return value || fallback;
  }

  function theme() {
    return {
      available: "#F7F9FC", availableStroke: "#8FA3B8",
      hovered: "#F2E3AE", hoveredStroke: "#8A7430",
      unavailable: "#CBD3DC", unavailableStroke: "#A9BCCE",
      selected: cssVar("--p-destaque", "#1351B4"),
      selectedStroke: cssVar("--p-destaque-hover", "#0C3F91"),
      markerStroke: "#5F7285",
      oceanFrom: "#EAF2F8", oceanTo: "#D8E4EE", oceanStroke: "#A9BCCE",
      graticule: "rgba(125,145,168,0.35)",
      land: "#EFECE4", landStroke: "rgba(125,145,168,0.45)",
      malha: cssVar("--p-linha", "#FFFFFF")
    };
  }

  function geometry(message) {
    const collection = JSON.parse(message.geojson);
    if (!collection || !Array.isArray(collection.features) || !collection.features.length) {
      throw new Error("Geometria das localidades ausente");
    }
    return collection.features.map(feature => ({
      feature: feature,
      code: String((feature.properties && (feature.properties.code != null ? feature.properties.code : feature.properties.local_id)) || ""),
      center: (feature.properties && feature.properties.center) || geo().geoCentroid(feature),
      area: geo().geoArea(feature)
    }));
  }

  // Uma feicao unica (destaque ou contexto) vinda de string GeoJSON.
  function parseGeojsonFeature(text) {
    const collection = JSON.parse(text);
    const feature = collection && Array.isArray(collection.features) &&
      collection.features.length ? collection.features[0] : null;
    if (!feature) return null;
    const props = feature.properties || {};
    return {
      feature: feature,
      code: String((props.code != null ? props.code : props.local_id) || ""),
      label: props.label != null ? String(props.label) : "",
      center: props.center || geo().geoCentroid(feature),
      area: geo().geoArea(feature)
    };
  }

  // Feicoes cruas de uma string GeoJSON — a malha municipal decorativa
  // (so bordas) dispensa code/label/center.
  function parseGeojsonFeatures(text) {
    const collection = JSON.parse(text);
    if (!collection || !Array.isArray(collection.features)) return [];
    return collection.features.filter(feature => feature && feature.geometry);
  }

  function element(tag, className, parent) {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (parent) parent.appendChild(node);
    return node;
  }

  function create(host) {
    const events = new root.AbortController();
    const signal = events.signal;
    const reducedMotion = root.matchMedia && root.matchMedia("(prefers-reduced-motion: reduce)");
    const state = {
      host: host, ufs: new Map(), disponiveis: new Set(), selected: "",
      inputId: latest ? String(latest.inputId || "") : "",
      readyId: latest ? String(latest.readyId || "") : "",
      localClick: null,
      features: [], rotation: [53.5, 10.5, 0], width: 360, radius: 160,
      zoom: 1, world: [], modo: "uf",
      destaque: null, destaqueCode: "", contexto: null, contextoCode: "",
      malha: [],
      frame: 0, animation: null, pointer: null, hovered: null, error: false,
      disposed: false, loaded: false, received: false
    };
    const text = key => labels[key];
    host.replaceChildren();
    host.classList.add("painel-globo");
    host.dataset.animating = "false";
    host.dataset.ready = "loading";
    host.dataset.received = "false";
    const viewport = element("div", "painel-globo-viewport", host);
    viewport.style.position = "relative";
    const canvas = element("canvas", "painel-globo-canvas", viewport);
    canvas.style.cssText = "display:block;width:100%;aspect-ratio:1;touch-action:none;cursor:grab;";
    canvas.setAttribute("role", "img");
    canvas.setAttribute("aria-label", text("globe"));
    const context = canvas.getContext("2d");
    const projection = geo() ? geo().geoOrthographic().precision(0.35).clipAngle(90) : null;
    const path = projection && context ? geo().geoPath(projection, context) : null;
    const graticule = geo() ? geo().geoGraticule().step([30, 30])() : null;
    const tooltip = element("div", "painel-globo-tooltip", viewport);
    tooltip.hidden = true;
    tooltip.setAttribute("role", "tooltip");
    tooltip.style.cssText = "position:absolute;pointer-events:none;max-width:calc(100% - 20px);z-index:2;";
    const status = element("div", "painel-globo-status", host);
    status.setAttribute("role", "status");
    status.setAttribute("aria-live", "polite");
    const instructions = element("p", "painel-globo-instrucao", host);
    const controls = element("div", "painel-globo-controles", host);
    const buttonSpecs = [["west", "\u2190", 16, 0], ["north", "\u2191", 0, -12],
      ["south", "\u2193", 0, 12], ["east", "\u2192", -16, 0], ["reset", "\u21ba", 0, 0],
      ["zoomIn", "+"], ["zoomOut", "\u2212"]];
    const buttons = buttonSpecs.map(spec => {
      const button = element("button", "painel-globo-controle", controls);
      button.type = "button";
      button.dataset.direction = spec[0];
      button.textContent = spec[1];
      button.addEventListener("click", () => {
        if (spec[0] === "reset") focusSelection();
        else if (spec[0] === "zoomIn") zoomBy(1.35);
        else if (spec[0] === "zoomOut") zoomBy(1 / 1.35);
        else {
          // O passo de rotacao diminui com o zoom para manter precisao.
          const fator = Math.max(1, state.zoom / 4);
          rotate(spec[2] / fator, spec[3] / fator);
        }
      }, { signal: signal });
      return button;
    });
    const retry = element("button", "painel-globo-retry", host);
    retry.type = "button";
    retry.hidden = true;
    retry.addEventListener("click", load, { signal: signal });
    const legend = element("div", "painel-globo-legenda", host);
    const legendItems = ["available", "selected", "unavailable"].map(key => {
      const item = element("span", "painel-globo-item", legend);
      element("i", "painel-globo-amostra painel-globo-amostra-" + key, item).setAttribute("aria-hidden", "true");
      return { label: element("span", "", item), key: key };
    });
    const attribution = element("small", "painel-globo-atribuicao", host);
    attribution.textContent = "Geometrias: IBGE · mundo: pacote maps · banco de dados do painel";

    function ufLabel(item) {
      if (!item) return text("ocean");
      return state.ufs.get(item.code) || item.code;
    }

    function selectionStatus() {
      if (state.error) return text("error");
      if (!state.loaded || !state.received) return text("loading");
      if (state.destaque) {
        return text("selected") + ": " +
          (state.destaque.label || state.destaqueCode);
      }
      if (!state.disponiveis.size) return text("empty");
      const selected = state.features.find(item => item.code === state.selected);
      const name = state.ufs.get(state.selected) || (selected ? ufLabel(selected) : "");
      return name ? text("selected") + ": " + name : "";
    }

    function translate() {
      host.setAttribute("aria-busy", String(!state.error && (!state.loaded || !state.received)));
      instructions.textContent = text("instruction");
      buttons.forEach((button, index) => {
        button.setAttribute("aria-label", text(buttonSpecs[index][0]));
        button.title = text(buttonSpecs[index][0]);
        button.disabled = !state.loaded || !state.received;
      });
      retry.textContent = text("retry");
      legendItems.forEach(item => { item.label.textContent = text(item.key); });
      status.textContent = selectionStatus();
    }

    function updateProjection() {
      state.radius = state.width * 0.455 * state.zoom;
      projection.rotate(state.rotation).translate([state.width / 2, state.width / 2]).scale(state.radius);
      // Resampling mais grosso em zoom alto mantem o redesenho leve.
      const precisao = clamp(0.35 * state.zoom / 4, 0.35, 6);
      if (projection.precision() !== precisao) projection.precision(precisao);
      host.dataset.longitude = (-state.rotation[0]).toFixed(4);
      host.dataset.latitude = (-state.rotation[1]).toFixed(4);
    }

    function drawFeature(item, fill, stroke, lineWidth) {
      context.beginPath();
      path(item.feature);
      context.fillStyle = fill;
      context.fill();
      if (stroke) {
        context.strokeStyle = stroke;
        context.lineWidth = lineWidth;
        context.stroke();
      }
    }

    function draw(timestamp) {
      state.frame = 0;
      if (state.disposed || !projection || !context) return;
      if (state.animation) {
        const animation = state.animation;
        const progress = reducedMotion && reducedMotion.matches ? 1 :
          clamp((timestamp - animation.started) / 780, 0, 1);
        const eased = progress < 0.5 ? 4 * progress * progress * progress :
          1 - Math.pow(-2 * progress + 2, 3) / 2;
        state.rotation = [clamp(animation.from[0] + animation.delta[0] * eased, LAMBDA[0], LAMBDA[1]),
          clamp(animation.from[1] + animation.delta[1] * eased, PHI[0], PHI[1]), 0];
        if (animation.zoomTo != null) {
          state.zoom = clamp(animation.zoomFrom *
            Math.pow(animation.zoomTo / animation.zoomFrom, eased), ZOOM[0], ZOOM[1]);
        }
        if (progress === 1) stopAnimation();
      }
      updateProjection();
      context.clearRect(0, 0, state.width, state.width);
      const colors = theme();
      const mid = state.width / 2;
      const ocean = context.createRadialGradient(mid * 0.65, mid * 0.6, state.radius * 0.1,
        mid, mid, state.radius);
      ocean.addColorStop(0, colors.oceanFrom);
      ocean.addColorStop(1, colors.oceanTo);
      context.beginPath();
      path({ type: "Sphere" });
      context.fillStyle = ocean;
      context.fill();
      context.strokeStyle = colors.oceanStroke;
      context.lineWidth = 0.8;
      context.stroke();
      context.beginPath();
      path(graticule);
      context.strokeStyle = colors.graticule;
      context.lineWidth = 0.55;
      context.stroke();

      // Massas de terra continentais sob as UFs (asset local opcional).
      if (state.world.length) {
        context.beginPath();
        state.world.forEach(feature => { path(feature); });
        context.fillStyle = colors.land;
        context.fill();
        context.strokeStyle = colors.landStroke;
        context.lineWidth = 0.4;
        context.stroke();
      }

      // Contexto (UF que contem a selecao): preenchimento leve e contorno
      // forte por baixo das feicoes — o estado aparece inteiro em foco.
      if (state.contexto) {
        context.save();
        context.globalAlpha = 0.10;
        drawFeature(state.contexto, colors.selected, null, 0);
        context.restore();
        drawFeature(state.contexto, "rgba(0,0,0,0)", colors.selectedStroke, 1.6);
      }

      const selected = state.features.find(item => item.code === state.selected);
      state.features.forEach(item => {
        if (item === selected) return;
        const available = state.disponiveis.has(item.code);
        const hovered = state.hovered === item;
        drawFeature(item, hovered && available ? colors.hovered : available ? colors.available : colors.unavailable,
          hovered && available ? colors.hoveredStroke : available ? colors.availableStroke : colors.unavailableStroke,
          hovered && available ? 1.2 : 0.55);
      });
      if (selected) drawFeature(selected, colors.selected, colors.selectedStroke, 0.9);

      // Malha municipal da UF em foco: so as bordas dos vizinhos, num
      // unico traco fino entre a base e o destaque.
      if (state.malha.length) {
        context.beginPath();
        state.malha.forEach(feature => { path(feature); });
        context.strokeStyle = colors.malha;
        context.lineWidth = 0.5;
        context.stroke();
      }

      // Destaque (localidade fora das feicoes da base): por cima de tudo.
      if (state.destaque) {
        drawFeature(state.destaque, colors.selected, colors.selectedStroke, 1.3);
      }

      // UFs de area muito pequena (caso do DF) continuam clicaveis nesta
      // escala, usando o ponto do centro quando a projecao teria poucos px.
      const center = projection.invert([mid, mid]);
      state.features.forEach(item => {
        if (!state.disponiveis.has(item.code) || item.area * state.radius * state.radius > 18) return;
        if (geo().geoDistance(center, item.center) > Math.PI / 2 - 0.025) return;
        const point = projection(item.center);
        if (!point || !point.every(Number.isFinite)) return;
        context.beginPath();
        context.arc(point[0], point[1], item === selected ? 3.8 : 3.1, 0, Math.PI * 2);
        context.fillStyle = item === selected ? colors.selected :
          state.hovered === item ? colors.hovered : colors.available;
        context.fill();
        context.strokeStyle = item === selected ? colors.selectedStroke : colors.markerStroke;
        context.lineWidth = 0.8;
        context.stroke();
      });
      if (state.animation) schedule();
    }

    function schedule() {
      if (!state.frame && !state.disposed) state.frame = root.requestAnimationFrame(draw);
    }

    function resize() {
      if (state.disposed || !context) return;
      const width = Math.max(1, host.getBoundingClientRect().width);
      if (width < 2) return;
      state.width = width;
      const ratio = Math.min(root.devicePixelRatio || 1, 3);
      canvas.width = Math.round(width * ratio);
      canvas.height = Math.round(width * ratio);
      context.setTransform(ratio, 0, 0, ratio, 0, 0);
      schedule();
    }

    function hideTooltip() {
      tooltip.hidden = true;
      if (state.hovered) {
        state.hovered = null;
        schedule();
      }
    }

    function pointFor(event) {
      const bounds = canvas.getBoundingClientRect();
      return [(event.clientX - bounds.left) * state.width / bounds.width,
        (event.clientY - bounds.top) * state.width / bounds.height];
    }

    function ufAt(point) {
      if (!state.loaded || !state.received) return null;
      const mid = state.width / 2;
      if (Math.hypot(point[0] - mid, point[1] - mid) > state.radius) return null;
      updateProjection();
      // A area de toque dos marcadores e delimitada: nenhum clique no oceano
      // seleciona uma UF distante apenas porque ela tem dados.
      const center = projection.invert([mid, mid]);
      const marker = state.features.filter(item => state.disponiveis.has(item.code) &&
        item.area * state.radius * state.radius <= 18 &&
        geo().geoDistance(center, item.center) <= Math.PI / 2 - 0.025)
        .map(item => ({ item: item, point: projection(item.center) }))
        .map(mark => ({ mark: mark,
          distance: Math.hypot(mark.point[0] - point[0], mark.point[1] - point[1]) }))
        .filter(entry => entry.distance <= 6).sort((a, b) => a.distance - b.distance)[0];
      if (marker) return marker.mark.item;
      const location = projection.invert(point);
      if (!location || !location.every(Number.isFinite)) return null;
      return state.features.find(item => geo().geoContains(item.feature, location)) || null;
    }

    function preview(point) {
      const item = ufAt(point);
      if (state.hovered !== item) {
        state.hovered = item;
        schedule();
      }
      canvas.style.cursor = item && state.disponiveis.has(item.code) ? "pointer" : "grab";
      if (!item) { tooltip.hidden = true; return; }
      tooltip.textContent = ufLabel(item) + (state.disponiveis.has(item.code) ? "" : " \u00b7 " + text("unavailable"));
      tooltip.hidden = false;
      const left = clamp(point[0] + 12, 8, Math.max(8, state.width - tooltip.offsetWidth - 8));
      const top = clamp(point[1] + 12, 8, Math.max(8, state.width - tooltip.offsetHeight - 8));
      tooltip.style.left = left + "px";
      tooltip.style.top = top + "px";
    }

    function stopAnimation() {
      state.animation = null;
      host.dataset.animating = "false";
    }

    function rotate(longitude, latitude) {
      if (!state.loaded || !state.received) return;
      stopAnimation();
      state.rotation[0] = clamp(state.rotation[0] + longitude, LAMBDA[0], LAMBDA[1]);
      state.rotation[1] = clamp(state.rotation[1] + latitude, PHI[0], PHI[1]);
      hideTooltip();
      schedule();
    }

    // Zoom estilo Google Earth: aumenta o raio da projecao ortografica
    // mantendo o centro (a rotacao corrente) — o mundo aproxima do olho.
    function zoomBy(factor) {
      const zoom = clamp(state.zoom * factor, ZOOM[0], ZOOM[1]);
      if (zoom === state.zoom) return;
      stopAnimation();
      state.zoom = zoom;
      schedule();
    }

    // Zoom que faz a feicao caber no disco: mede a extensao projetada
    // com scale de referencia (zoom 1) e reescala para ocupar ~80% do raio.
    function fitZoom(item) {
      if (!item) return state.zoom;
      projection.rotate([-item.center[0], -item.center[1], 0])
        .translate([state.width / 2, state.width / 2])
        .scale(state.width * 0.455);
      const b = geo().geoPath(projection).bounds(item.feature);
      const half = Math.max(b[1][0] - b[0][0], b[1][1] - b[0][1]) / 2;
      if (!Number.isFinite(half) || half <= 1) return state.zoom;
      return clamp((state.width * 0.40) / half, ZOOM[0], ZOOM[1]);
    }

    // Centraliza e aproxima ate o alvo caber: a feicao selecionada, ou a
    // UF de contexto quando a selecao e um destaque (municipio dentro do
    // estado). Preserva a orientacao corrente e usa o arco mais curto.
    function focusSelection(animate = true, alvo = null) {
      alvo = alvo || state.contexto ||
        state.features.find(feature => feature.code === state.selected) || null;
      if (state.animation && alvo && state.animation.alvo === alvo.code) return;
      stopAnimation();
      if (alvo) {
        const delta = [clamp(-alvo.center[0], LAMBDA[0], LAMBDA[1]) - state.rotation[0],
          -alvo.center[1] - state.rotation[1]];
        const zoomTo = fitZoom(alvo);
        if (animate && !(reducedMotion && reducedMotion.matches) &&
            (Math.hypot(delta[0], delta[1]) > 0.05 ||
             Math.abs(zoomTo - state.zoom) > 0.01)) {
          state.animation = { alvo: alvo.code, from: state.rotation.slice(), delta: delta,
            zoomFrom: state.zoom, zoomTo: zoomTo, started: root.performance.now() };
          host.dataset.animating = "true";
        } else {
          state.rotation = [clamp(-alvo.center[0], LAMBDA[0], LAMBDA[1]),
            clamp(-alvo.center[1], PHI[0], PHI[1]), 0];
          state.zoom = zoomTo;
        }
      }
      hideTooltip();
      schedule();
      status.textContent = selectionStatus();
    }

    function select(item) {
      if (!item) return;
      if (!state.disponiveis.has(item.code)) {
        status.textContent = ufLabel(item) + " \u00b7 " + text("unavailable");
        return;
      }
      // Inicia imediatamente; o eco do Shiny nao deve reiniciar a rotacao.
      if (state.modo !== "uf") state.selected = item.code;
      state.localClick = { code: item.code, at: Date.now() };
      host.dataset.selected = item.code;
      focusSelection(true, item);
      if (root.Shiny && root.Shiny.setInputValue && state.inputId) {
        root.Shiny.setInputValue(state.inputId,
          { code: item.code, modo: state.modo }, { priority: "event" });
      }
    }

    canvas.addEventListener("pointerdown", event => {
      if (!state.loaded || !state.received || state.pointer || event.button !== 0) return;
      const point = pointFor(event);
      if (Math.hypot(point[0] - state.width / 2, point[1] - state.width / 2) > state.radius) return;
      stopAnimation();
      state.pointer = { id: event.pointerId, point: point, rotation: state.rotation.slice(), moved: false };
      canvas.setPointerCapture(event.pointerId);
      canvas.style.cursor = "grabbing";
      hideTooltip();
    }, { signal: signal });
    canvas.addEventListener("pointermove", event => {
      const point = pointFor(event);
      if (!state.pointer) { if (event.pointerType !== "touch") preview(point); return; }
      if (event.pointerId !== state.pointer.id) return;
      const dx = point[0] - state.pointer.point[0];
      const dy = point[1] - state.pointer.point[1];
      if (Math.hypot(dx, dy) > 5) state.pointer.moved = true;
      if (!state.pointer.moved) return;
      const speed = 100 / state.radius;
      state.rotation = [clamp(state.pointer.rotation[0] + dx * speed, LAMBDA[0], LAMBDA[1]),
        clamp(state.pointer.rotation[1] - dy * speed, PHI[0], PHI[1]), 0];
      schedule();
    }, { signal: signal });
    canvas.addEventListener("pointerup", event => {
      if (!state.pointer || event.pointerId !== state.pointer.id) return;
      const pointer = state.pointer;
      state.pointer = null;
      if (canvas.hasPointerCapture(event.pointerId)) canvas.releasePointerCapture(event.pointerId);
      canvas.style.cursor = "grab";
      if (!pointer.moved) select(ufAt(pointFor(event)));
    }, { signal: signal });
    function cancelPointer() {
      state.pointer = null;
      canvas.style.cursor = "grab";
      hideTooltip();
    }
    canvas.addEventListener("pointercancel", cancelPointer, { signal: signal });
    canvas.addEventListener("lostpointercapture", cancelPointer, { signal: signal });
    canvas.addEventListener("pointerleave", () => { if (!state.pointer) hideTooltip(); }, { signal: signal });
    canvas.addEventListener("wheel", event => {
      if (!state.loaded || !state.received) return;
      event.preventDefault();
      zoomBy(Math.exp(-event.deltaY * 0.0016));
    }, { signal: signal, passive: false });

    const resizeObserver = root.ResizeObserver ? new root.ResizeObserver(resize) : null;
    if (resizeObserver) resizeObserver.observe(host);
    else root.addEventListener("resize", resize, { signal: signal });

    function load() {
      state.error = false;
      host.dataset.ready = "loading";
      retry.hidden = true;
      host.setAttribute("aria-busy", "true");
      translate();
      if (!geo() || !context) { failed(); return; }
      if (!latest || !latest.geojson) return; // aguarda a mensagem do servidor
      try {
        const features = geometry(latest);
        state.features = features;
        state.loaded = true;
        host.dataset.ready = "true";
        host.dataset.featureCount = String(features.length);
        host.setAttribute("aria-busy", "false");
        translate();
        resize();
        focusSelection(false);
      } catch (error) {
        failed();
      }
    }

    // Contorno mundial de fundo (asset local; falha silenciosa).
    function loadWorld() {
      if (state.world.length) return;
      fetch("painel_recursos/painel-mundo.geojson").then(response => {
        if (!response.ok) throw new Error("contorno mundial indisponivel");
        return response.json();
      }).then(collection => {
        if (state.disposed) return;
        state.world = collection && Array.isArray(collection.features) ?
          collection.features : [];
        schedule();
      }).catch(() => { /* sem mundo, segue esfera + graticule + UFs */ });
    }

    function failed() {
      if (state.disposed) return;
      state.error = true;
      host.dataset.ready = "error";
      host.setAttribute("aria-busy", "false");
      retry.hidden = false;
      translate();
    }

    function update(message) {
      const previous = state.destaqueCode || state.selected;
      state.received = true;
      host.dataset.received = "true";
      if (message.inputId) state.inputId = String(message.inputId);
      if (message.readyId) state.readyId = String(message.readyId);
      state.modo = String(message.modo || "uf");
      if (message.geojson) {
        // Troca de nivel territorial: substitui as feicoes da base.
        try {
          state.features = geometry(message);
        } catch (error) { /* mantem as feicoes atuais */ }
        if (!state.loaded) {
          state.loaded = true;
          host.dataset.ready = "true";
          host.dataset.featureCount = String(state.features.length);
          host.setAttribute("aria-busy", "false");
          resize();
        }
      }
      const entries = Array.isArray(message.locais) ? message.locais :
        Array.isArray(message.ufs) ? message.ufs : [];
      state.ufs = new Map(entries.filter(uf => uf && uf.code != null)
        .map(uf => [String(uf.code), String(uf.label || uf.code)]));
      state.disponiveis = new Set((Array.isArray(message.disponiveis) ? message.disponiveis : [])
        .filter(code => code != null).map(String));
      // Destaque (localidade fora das feicoes) e contexto (UF que a
      // contem): a geometria viaja quando muda; o cliente guarda a ultima.
      state.destaqueCode = String(message.destaque || "");
      if (message.destaqueGeojson) {
        state.destaque = parseGeojsonFeature(message.destaqueGeojson);
      } else if (!state.destaqueCode) state.destaque = null;
      if (state.destaque && state.destaqueCode) state.destaque.code = state.destaqueCode;
      if (!state.destaqueCode) state.destaque = null;
      state.contextoCode = String(message.contexto || "");
      if (message.contextoGeojson) {
        state.contexto = parseGeojsonFeature(message.contextoGeojson);
      } else if (!state.contextoCode) state.contexto = null;
      if (state.contexto && state.contextoCode) state.contexto.code = state.contextoCode;
      if (!state.contextoCode) state.contexto = null;
      // Malha municipal acompanha o contexto: viaja com a UF em foco e
      // some junto quando nao ha selecao municipal.
      if (message.malhaGeojson) {
        try {
          state.malha = parseGeojsonFeatures(message.malhaGeojson);
        } catch (error) { /* mantem a malha atual */ }
      } else if (!state.contextoCode) state.malha = [];
      // A selecao local pode estar varios cliques a frente desta mensagem do
      // servidor; mantem-a quando ainda valida para a disponibilidade recebida.
      const local = state.localClick;
      const localFresh = local && Date.now() - local.at < 600 && state.disponiveis.has(local.code);
      const server = String(message.selected || "");
      state.selected = state.modo === "uf" ? "" :
        localFresh ? local.code :
        state.disponiveis.has(server) ? server : "";
      host.dataset.selected = state.destaqueCode || state.selected;
      host.dataset.availableCount = String(state.disponiveis.size);
      translate();
      if (!state.loaded && latest && latest.geojson) load();
      else if (state.loaded && previous !== (state.destaqueCode || state.selected)) {
        focusSelection(Boolean(previous));
      }
      hideTooltip();
      schedule();
    }

    function destroy() {
      state.disposed = true;
      stopAnimation();
      events.abort();
      if (resizeObserver) resizeObserver.disconnect();
      if (state.frame) root.cancelAnimationFrame(state.frame);
      host.replaceChildren();
    }

    if (latest) update(latest);
    load();
    loadWorld();
    // Handshake: o servidor reenvia a geometria apos cada remontagem do host.
    if (state.readyId && root.Shiny && root.Shiny.setInputValue) {
      root.Shiny.setInputValue(state.readyId, ++handshake, { priority: "event" });
    }
    return { host: host, update: update, destroy: destroy };
  }

  function findHost() {
    return document.querySelector(".painel-globo-host") ||
      (latest && latest.hostId ? document.getElementById(latest.hostId) : null);
  }

  function boot() {
    bootFrame = 0;
    if (!registered && root.Shiny && root.Shiny.addCustomMessageHandler) {
      root.Shiny.addCustomMessageHandler("painel-globe", receive);
      registered = true;
    }
    const host = findHost();
    if (controller && controller.host !== host) {
      controller.destroy();
      controller = null;
    }
    if (host && !controller) controller = create(host);
  }

  function scheduleBoot() {
    if (!bootFrame) bootFrame = root.requestAnimationFrame(boot);
  }

  function receive(message) {
    if (!message || typeof message !== "object") return;
    latest = message;
    boot();
    if (controller) controller.update(latest);
  }

  root.PainelGlobo = { update: receive, init: boot };
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot, { once: true });
  else boot();
  if (root.jQuery) root.jQuery(document).on("shiny:connected.painelGlobo shiny:bound.painelGlobo", boot);
  const observer = new root.MutationObserver(mutations => {
    if (mutations.some(mutation => Array.from(mutation.addedNodes).concat(Array.from(mutation.removedNodes))
      .some(node => node.nodeType === 1 && (node.classList && node.classList.contains("painel-globo-host") ||
        node.querySelector && node.querySelector(".painel-globo-host"))))) {
      scheduleBoot();
    }
  });
  observer.observe(document.documentElement, { childList: true, subtree: true });
})(window);
