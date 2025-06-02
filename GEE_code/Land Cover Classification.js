// === LAND COVER CLASSIFICATION APP WITH STEP-BY-STEP UI ===

// Classes and colors
var classNames = [
  // wetlands and open water
  'Open Water', 'Estuarine Open Water', 'Brackish Ponds', 'Aquaculture Ponds', 'Salt Production Ponds',
  'Freshwater Marsh', 'Seasonal Wetlands', 'Floodplain Wetlands', 'Salt Marsh', 'Mangroves',
  // shoreline and coastal habitats
  'Tidal Mudflats', 'Riverine Sandbars', 'Coastal Lagoons', 'Sandy Beaches', 'Gravel Beaches',
  'Rocky Shores', 'Sand Dunes', 'Dunes with Vegetation',
  // terrestrial vegetated types
  'Forest', 'Mixed Forest', 'Savanna', 'Grassland', 'Shrubland', 'Plantation', 'Orchard', 'Peatlands', 'Desert',
  // agriculture
  'Cropland', 'Dry Croplands', 'Irrigated Croplands', 'Rice Fields', 'Fallow Rice Fields',
  // grasslands / pastures
  'Grasslands (Extensive Management)', 'Grasslands (Intensive Management)', 'Coastal Grasslands',
  // other land cover
  'Bare Soil', 'Rocky Area', 'Snow / Ice', 'Urban / Built-up',
  // placeholders
  'class1', 'class2', 'class3', 'class4', 'class5', 'class6', 'class7', 'class8', 'class9', 'class10',
  'class11', 'class12', 'class13', 'class14', 'class15', 'class16', 'class17', 'class18', 'class19', 'class20'
];

var classPalette = [
  // wetlands and open water
  '#0571b0', '#3690c0', '#78c679', '#66bd63', '#31a354',
  '#1b7837', '#5aae61', '#4dac26', '#2c7bb6', '#41ab5d',
  // shoreline and coastal habitats
  '#c7e9b4', '#f7fcb9', '#addd8e', '#fee08b', '#e0c080',
  '#bdb76b', '#d9d9d9', '#a6bddb',
  // terrestrial vegetated types
  '#006400', '#228b22', '#8fbc8f', '#66c2a5', '#7f6000', '#4d9221', '#b2df8a', '#8c510a', '#deb887',
  // agriculture
  '#ff7f50', '#f781bf', '#fb9a99', '#e7298a', '#d95f02',
  // grasslands / pastures
  '#984ea3', '#6a3d9a', '#a6cee3',
  // other land cover
  '#d2b48c', '#8c8c8c', '#f0f0f0', '#ff00ff',
  // placeholders
  '#00bfc4', '#0173b2', '#de8f05', '#029e73', '#d55e00', '#cc78bc', '#ca9161', '#fbafe4', '#949494', '#8dd3c7',
  '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#bc80bd', '#ccebc5', '#ffed6f'
];


// State variables
var currentYear = 2025;
var currentMonth = 'All Months';
var monthNames = ['All Months', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
var points = [];
var classifier = null;
var classified = null;
var clipped = null;
var legend = null;
var awaitingExportClick = false;
var classifiedLayer = null;

// Composite function
function getLandsatComposite(year, month) {
  var monthIndex = monthNames.indexOf(month);
  var startDate, endDate;
  if (monthIndex === 0) {
    startDate = ee.Date.fromYMD(year, 1, 1);
    endDate = ee.Date.fromYMD(year, 12, 31);
  } else {
    startDate = ee.Date.fromYMD(year, monthIndex, 1);
    endDate = startDate.advance(1, 'month');
  }

  // Load both Landsat 8 and Landsat 9 collections
  var landsat8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    .filterDate(startDate, endDate)
    .filter(ee.Filter.lt('CLOUD_COVER', 20));
    
  var landsat9 = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
    .filterDate(startDate, endDate)
    .filter(ee.Filter.lt('CLOUD_COVER', 20));

  // Merge the collections
  var combinedCollection = landsat8.merge(landsat9)
    .map(function(image) {
      var sr = image.select(['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7']).multiply(0.0000275).add(-0.2);
      var ndvi = sr.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI');
      return sr.addBands(ndvi);
    });

  return combinedCollection.median();
}


// UI elements
var yearDropdown = ui.Select({
  items: ee.List.sequence(2015, 2025).getInfo().map(String).reverse(),
  value: String(currentYear),
  onChange: function(val) {
    currentYear = parseInt(val);
    updateLandsatImage();
  }
});

var monthDropdown = ui.Select({
  items: monthNames,
  value: currentMonth,
  onChange: function(val) {
    currentMonth = val;
    updateLandsatImage();
  }
});

var dropdown = ui.Select({items: classNames, placeholder: 'Select Class'});
var info = ui.Label('Selected Class: none');
dropdown.onChange(function(val) {
  info.setValue('Selected Class: ' + val);
  Map.style().set('cursor', val ? 'crosshair' : 'hand');
});

var clearPointsButton = ui.Button('Clear Points', function() {
  points = [];
  redrawPoints();
});

var classifyButton = ui.Button('Classify Map', function() {
  if (points.length === 0) {
    ui.alert('Please add training points before proceeding.');
    return;
  }
  var trainingFC = ee.FeatureCollection(points.map(function(p) {
    return ee.Feature(ee.Geometry.Point([p.lon, p.lat]), {'class': p.label});
  }));
  var landsat = getLandsatComposite(currentYear, currentMonth);
  var bands = ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'];
  var training = landsat.select(bands).sampleRegions({
    collection: trainingFC,
    properties: ['class'],
    scale: 30
  });
  classifier = ee.Classifier.smileRandomForest(50).train({
    features: training,
    classProperty: 'class',
    inputProperties: bands
  });
  classified = landsat.select(bands).classify(classifier);
  if (classifiedLayer) Map.layers().remove(classifiedLayer);
  classifiedLayer = ui.Map.Layer(classified, {min: 0, max: classNames.length - 1, palette: classPalette}, 'Classified');
  Map.layers().add(classifiedLayer);
  selectExportButton.setDisabled(false);

  // Update legend
  ui.root.widgets().forEach(function(widget) {
    if (widget instanceof ui.Panel && widget.style().get('position') === 'bottom-left') {
      ui.root.remove(widget);
    }
  });
  legend = ui.Panel({style: {position: 'bottom-left'}});
  legend.add(ui.Label('Legend'));
  var usedClasses = points
    .map(function(p) { return p.label; })
    .filter(function(value, index, self) { return self.indexOf(value) === index; });
  usedClasses.forEach(function(index) {
    var colorBox = ui.Label('', {backgroundColor: classPalette[index], padding: '8px', margin: '4px'});
    var label = ui.Label(classNames[index]);
    legend.add(ui.Panel([colorBox, label], ui.Panel.Layout.Flow('horizontal')));
  });
  ui.root.add(legend);
});

var selectExportButton = ui.Button({
  label: 'Select Export Area',
  disabled: true,
  onClick: function() {
    awaitingExportClick = true;
    ui.notify('Click on the map to define export area (20 km buffer).');
  }
});

var resetButton = ui.Button({
  label: 'Reset App',
  style: {stretch: 'horizontal', color: 'black', backgroundColor: '#d9534f'},  // red button
  onClick: function() {
    // Clear points and layers
    points = [];
    redrawPoints();
    Map.layers().reset();
    
    // Reset classifier and state
    classifier = null;
    classified = null;
    clipped = null;
    classifiedLayer = null;
    awaitingExportClick = false;
    
    // Reload the Landsat composite
    updateLandsatImage();
    
    // Remove legend if it exists
    ui.root.widgets().forEach(function(widget) {
      if (widget instanceof ui.Panel && widget.style().get('position') === 'bottom-left') {
        ui.root.remove(widget);
      }
    });

    yearDropdown.setValue('2025');
    monthDropdown.setValue('All Months');
    dropdown.setValue(null);
    info.setValue('Selected Class: none');
    
    statusLabel.setValue('App reset. Select year, month, and start again.');
  }
});


// === STEP-BY-STEP UI PANEL ===
var panel = ui.Panel({style: {width: '300px'}});
ui.root.insert(0, panel);

var title = ui.Label({
  value: 'Land Cover Classification App',
  style: {
    fontWeight: 'bold',
    fontSize: '20px',
    margin: '10px 0 10px 0',
    color: '#1f78b4'
  }
});


// Step panels:
var step1 = ui.Panel([
  ui.Label('Step 1: Select the year and (optionally) the month to generate the Landsat 8/9 composite image.'),
  ui.Label('Select Year:', {fontWeight: 'bold'}),
  yearDropdown,
  ui.Label('Select Month:', {fontWeight: 'bold'}),
  monthDropdown
], null, {margin: '10px 0'});

var step2 = ui.Panel([
  ui.Label('Step 2: Choose a land cover class and click the map to create training points. Points may take a moment to appear, but you can continue placing additional points without waiting. class1 to class20 are placeholder labels. You can use these to manually assign custom class names later in your analysis.'),
  dropdown,
  info,
  clearPointsButton
], null, {margin: '10px 0'});

var step3 = ui.Panel([
  ui.Label('Step 3: When you have finished adding training points, click "Classify Map" to run the classification.'),
  classifyButton
], null, {margin: '10px 0'});

var step4 = ui.Panel([
  ui.Label('Step 4: After classification completes, click "Select Export Area," then click on the map to define the export region (40 km x 40 km).'),
  selectExportButton
], null, {margin: '10px 0'});

var resetPanel = ui.Panel([
  resetButton
], null, {margin: '10px 0'});

panel.add(title);
panel.add(resetPanel);

panel.add(step1);
panel.add(step2);
panel.add(step3);
panel.add(step4);



// Handle map clicks
Map.onClick(function(coords) {
  if (!classifier) {
    var selected = dropdown.getValue();
    if (!selected) return;
    points.push({lon: coords.lon, lat: coords.lat, label: classNames.indexOf(selected)});
    redrawPoints();
  } else if (classifier && awaitingExportClick) {
    var clickedPoint = ee.Geometry.Point([coords.lon, coords.lat]);
    var buffer = clickedPoint.buffer(20000);
    var exportRegion = buffer.bounds();
    clipped = classified.clip(exportRegion);
    Map.layers().reset();
    var clippedLayer = ui.Map.Layer(clipped, {min: 0, max: classNames.length - 1, palette: classPalette}, 'Clipped Export Area');
    Map.layers().add(clippedLayer);
    Map.centerObject(buffer, 10);
    generateDownloadURL(clipped, exportRegion);
  }
});

// Redraw training points
function redrawPoints() {
  var fc = ee.FeatureCollection(points.map(function(p) {
    return ee.Feature(ee.Geometry.Point([p.lon, p.lat]), {'class': p.label});
  }));
  var layers = Map.layers();
  for (var i = 0; i < layers.length(); i++) {
    if (layers.get(i).getName() === 'Training Points') {
      Map.layers().remove(layers.get(i));
      break;
    }
  }
  var pointsLayer = ui.Map.Layer(fc, {color: 'FF0000'}, 'Training Points');
  Map.layers().add(pointsLayer);
}

// Download URL
function generateDownloadURL(image, region) {
  image.getDownloadURL({
    name: 'landcover_export',
    region: region,
    scale: 30,
    maxPixels: 1e9
  }, function(url) {
    panel.add(ui.Label('Download your clipped image:', {margin: '10px 0 0 0'}));
    panel.add(ui.Label(url, {color: 'blue'}, url));
  });
}

// Load and display Landsat composite
function updateLandsatImage() {
  var landsat = getLandsatComposite(currentYear, currentMonth);
  var layers = Map.layers();
  for (var i = 0; i < layers.length(); i++) {
    if (layers.get(i).getName() === 'Landsat') {
      Map.layers().remove(layers.get(i));
      break;
    }
  }
  Map.layers().add(ui.Map.Layer(landsat, {bands: ['SR_B4', 'SR_B3', 'SR_B2'], min: 0, max: 0.3}, 'Landsat'));
}

updateLandsatImage();
Map.drawingTools().setShown(false);