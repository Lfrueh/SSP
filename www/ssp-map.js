// Restyle polygons that are already on the map.
//
// The server draws the polygons once per (geography, state, counties) and then
// sends only colors, popup text and a dim flag when year, measure or the
// histogram brush change. That is far less data than redrawing every polygon.
//
// Message fields (one entry per area with data, matched by layerId = GEOID10):
//   id    output id of the leaflet map
//   ids   GEOID10 of each area
//   fill  fill color
//   popup popup html
//   dim   TRUE to fade the area (outside the histogram brush)
// Polygons whose id is not in `ids` have no data for the chosen year and are hidden.
Shiny.addCustomMessageHandler("ssp-restyle", function (msg) {
  var el = document.getElementById(msg.id);
  var map = el && $(el).data("leaflet-map");
  if (!map) return;

  // Shiny turns length-1 vectors into scalars, so normalise to arrays.
  var ids = [].concat(msg.ids);
  var fill = [].concat(msg.fill);
  var popup = [].concat(msg.popup);
  var dim = [].concat(msg.dim);

  var position = {};
  ids.forEach(function (id, i) {
    position[id] = i;
  });

  map.eachLayer(function (layer) {
    var id = layer.options && layer.options.layerId;
    if (!(layer instanceof L.Polygon) || id === undefined || id === null) return;

    var i = position[id];
    if (i === undefined) {
      layer.setStyle({ opacity: 0, fillOpacity: 0 });
      layer.unbindPopup();
      return;
    }

    layer.setStyle({
      fillColor: fill[i],
      fillOpacity: dim[i] ? 0.08 : 0.9,
      color: "gray",
      weight: 0.7,
      opacity: dim[i] ? 0.25 : 1,
    });

    if (layer.getPopup()) {
      layer.setPopupContent(popup[i]);
    } else {
      layer.bindPopup(popup[i]);
    }
  });
});
