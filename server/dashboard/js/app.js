const CHART_MAX_POINTS = 20;

window.EVENTS = {
  ENTRYPOINTS: 'entrypoints',
  DASHBOARD_READY: 'dashboard_ready',
  DASHBOARD_ERROR: 'dashboard_error',
  HEALTH_TICK: 'health_tick',
  HEALTH_ERROR: 'health_error',
};

// ------------ Helpers

window.dispatch = function dispatch(event, detail = {}) {
  window.dispatchEvent(new CustomEvent(event, { detail }));
}

window.sel = function sel(selector) {
  return document.querySelector(selector);
}

window.all = function all(selector) {
  return document.querySelectorAll(selector);
}


window.on = function on(event, callback) {
  window.addEventListener(event, ({ detail }) => {
    callback(detail)
  })
}

window.timestamp = function timestamp() {
  return (new Date()).toISOString().slice(11,-5);
}

window.addPoint = function addPoint(chart, x, y) {
  chart.data.labels.push(x);
  if (chart.data.labels.length > CHART_MAX_POINTS) {
    chart.data.labels = chart.data.labels.slice(1);
  }

  chart.data.datasets.forEach(dataset => {
    dataset.data.push(y);
    if (dataset.data.length > CHART_MAX_POINTS) {
      dataset.data = dataset.data.slice(1);
    }
  });

  chart.update();
}

window.uptime = function uptime(startTime) {
  const s = ((new Date()).getTime() - (new Date(startTime)).getTime()) / 1000;
  if (s < 60) {
    return `${Math.round(s)}s`;
  } else if (s < 120 * 60) {
    return `${Math.round(s / 60)}min`;
  } else {
    return `${Math.round(s / 3600)}h`;
  }
}

window.percentage = function percentage(val) {
  if (val == null) {
    return `?`;
  } else {
    return `${(100 * val).toFixed(2)}%`;
  }
}

window.debounce = function debounce(callback, delay) {
  let timeout;
  return (...args) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => { callback.apply(this, args); }, delay);
  };
}

async function fetchHealth(baseUrl) {
  const healthUrl = baseUrl + '/health'
  return fetch(healthUrl).then(response => response.json())
}

window.addEventListener('load', async function () {
  try {
    const host = window.location.host.replace('8000', '1337');
    const websocketProtocol = window.location.protocol === "http:" ? "ws" : "wss";
    const websocketUrl = websocketProtocol + "://" + host;
    const httpUrl = window.location.protocol + '//' + host;

    dispatch(EVENTS.ENTRYPOINTS, { websocketUrl, httpUrl });

    let loaded = false;

    setInterval(async () => {
      try {
        const health = await fetchHealth(httpUrl);
        if (!loaded) {
          loaded = true;
          dispatch(EVENTS.DASHBOARD_READY);
        }
        dispatch(EVENTS.HEALTH_TICK, health);
      } catch (e) {
        dispatch(EVENTS.DASHBOARD_ERROR, {
          error: "Failed to fetch server's health.",
          reason: e.message,
        });
      }
    }, 1000);
  } catch (e) {
    dispatch(EVENTS.DASHBOARD_ERROR, e);
  }
});
