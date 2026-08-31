// Sentinel-2 spectral index GEE app

/**** SETUP ****/
Map.setCenter(10, 50, 4);

var roi = null;
var selectedImage = null;
var selectedCollection = null;
var clipPolygon = null;
var chosenYear = null;
var chosenMonth = null;
var chosenSatellite = 'Sentinel-2';
var chosenIndex = 'GPI';
var chosenProduct = 'Single Scene (LEAST CLOUDY)';
var drawingStage = 'roi';
var requestVersion = 0;

var currentYear = new Date().getFullYear();
var rgbViz = {min: 0, max: 0.3, bands: ['red', 'green', 'blue']};

// Collections, dates, bands, and output resolution
var satelliteOptions = {
  'Sentinel-2': {
    collection: 'COPERNICUS/S2_SR_HARMONIZED',
    startYear: 2017,
    endYear: currentYear,
    cloudProperty: 'CLOUDY_PIXEL_PERCENTAGE',
    sourceBands: ['B2', 'B3', 'B4', 'B8', 'B11', 'B5', 'B6', 'B7'],
    outputBands: ['blue', 'green', 'red', 'nir', 'swir1',
      'redEdge1', 'redEdge2', 'redEdge3'],
    scale: 10,
    supportsGPI: true
  },
  'Landsat 7': {
    collection: 'LANDSAT/LE07/C02/T1_L2',
    startYear: 1999,
    endYear: 2024,
    cloudProperty: 'CLOUD_COVER',
    sourceBands: ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5'],
    outputBands: ['blue', 'green', 'red', 'nir', 'swir1'],
    scale: 30,
    supportsGPI: false
  },
  'Landsat 8': {
    collection: 'LANDSAT/LC08/C02/T1_L2',
    startYear: 2013,
    endYear: currentYear,
    cloudProperty: 'CLOUD_COVER',
    sourceBands: ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6'],
    outputBands: ['blue', 'green', 'red', 'nir', 'swir1'],
    scale: 30,
    supportsGPI: false
  },
  'Landsat 9': {
    collection: 'LANDSAT/LC09/C02/T1_L2',
    startYear: 2021,
    endYear: currentYear,
    cloudProperty: 'CLOUD_COVER',
    sourceBands: ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6'],
    outputBands: ['blue', 'green', 'red', 'nir', 'swir1'],
    scale: 30,
    supportsGPI: false
  }
};

function maskClouds(image) {
  if (chosenSatellite === 'Sentinel-2') {
    var scl = image.select('SCL');
    var s2Mask = scl.neq(0)
      .and(scl.neq(1))
      .and(scl.neq(3))
      .and(scl.neq(8))
      .and(scl.neq(9))
      .and(scl.neq(10))
      .and(scl.neq(11));
    return image.updateMask(s2Mask);
  }

  var qaMask = image.select('QA_PIXEL').bitwiseAnd(63).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);
  return image.updateMask(qaMask).updateMask(saturationMask);
}

function prepareImage(image, applyMask) {
  var satellite = satelliteOptions[chosenSatellite];
  image = applyMask ? maskClouds(image) : image;
  var reflectance = image.select(satellite.sourceBands, satellite.outputBands);

  if (chosenSatellite === 'Sentinel-2') {
    reflectance = reflectance.multiply(0.0001);
  } else {
    reflectance = reflectance.multiply(0.0000275).add(-0.2);
  }

  return ee.Image(reflectance.copyProperties(image, image.propertyNames()));
}

// Index formulas and display settings
var indexOptions = {
  GPI: {
    label: 'GPI - Grassland Production Intensity',
    viz: {min: 708, max: 741, palette: ['yellow', 'green', 'grey']},
    calculate: function(image) {
      var red = image.select('red');
      var redEdge1 = image.select('redEdge1');
      var redEdge2 = image.select('redEdge2');
      var redEdge3 = image.select('redEdge3');
      return ee.Image(705).add(
        ee.Image(35).multiply(
          redEdge3.add(red).divide(2).subtract(redEdge1)
        ).divide(redEdge2.subtract(redEdge1))
      ).rename('GPI');
    }
  },
  NDVI: {
    label: 'NDVI - Normalized Difference Vegetation Index',
    viz: {min: -0.2, max: 0.9, palette: ['brown', 'yellow', 'green']},
    calculate: function(image) {
      return image.expression('(nir - red) / (nir + red)', {
        nir: image.select('nir'),
        red: image.select('red')
      }).rename('NDVI');
    }
  },
  EVI: {
    label: 'EVI - Enhanced Vegetation Index',
    viz: {min: -0.2, max: 1, palette: ['brown', 'yellow', 'darkgreen']},
    calculate: function(image) {
      return image.expression(
        '2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))', {
          nir: image.select('nir'),
          red: image.select('red'),
          blue: image.select('blue')
        }
      ).rename('EVI');
    }
  },
  NDWI: {
    label: 'NDWI - Normalized Difference Water Index',
    viz: {min: -0.5, max: 0.8, palette: ['brown', 'white', 'blue']},
    calculate: function(image) {
      return image.expression('(green - nir) / (green + nir)', {
        green: image.select('green'),
        nir: image.select('nir')
      }).rename('NDWI');
    }
  },
  NDMI: {
    label: 'NDMI - Normalized Difference Moisture Index',
    viz: {min: -0.5, max: 0.8, palette: ['brown', 'yellow', 'blue']},
    calculate: function(image) {
      return image.expression('(nir - swir1) / (nir + swir1)', {
        nir: image.select('nir'),
        swir1: image.select('swir1')
      }).rename('NDMI');
    }
  },
  SAVI: {
    label: 'SAVI - Soil Adjusted Vegetation Index',
    viz: {min: -0.2, max: 1, palette: ['brown', 'yellow', 'green']},
    calculate: function(image) {
      return image.expression(
        '1.5 * ((nir - red) / (nir + red + 0.5))', {
          nir: image.select('nir'),
          red: image.select('red')
        }
      ).rename('SAVI');
    }
  },
  SWIR: {
    label: 'SWIR - Surface Water',
    viz: {min: 0, max: 0.3, palette: ['081d58', '41b6c4', 'ffffd9']},
    calculate: function(image) {
      return image.select('swir1').rename('SWIR');
    }
  }
};

var indexNames = Object.keys(indexOptions).filter(function(name) {
  return name !== 'SWIR';
});
var indexLabels = indexNames.map(function(name) {
  return indexOptions[name].label;
});

function calculateIndex(image, indexName) {
  return indexOptions[indexName].calculate(image);
}

function getMonthlyCollection(selectedYear, selectedMonth) {
  var startDate = ee.Date(selectedYear + '-' + selectedMonth + '-01');
  var endDate = startDate.advance(1, 'month');
  var satellite = satelliteOptions[chosenSatellite];

  return ee.ImageCollection(satellite.collection)
    .filterBounds(roi)
    .filterDate(startDate, endDate);
}

function getSelectedIndexImage() {
  if (chosenProduct === 'Monthly Median Mosaic' && selectedCollection) {
    return selectedCollection.map(function(image) {
      return calculateIndex(image, chosenIndex);
    }).median().rename(chosenIndex);
  }

  return calculateIndex(selectedImage, chosenIndex);
}

/**** UI PANEL ****/
var colors = {
  primary: '#24543d',
  primaryDark: '#173b2b',
  accent: '#d9a441',
  background: '#f4f6f3',
  card: '#ffffff',
  border: '#d6ddd7',
  text: '#26332c',
  muted: '#66736b'
};

var panel = ui.Panel({
  style: {
    width: '360px',
    padding: '0',
    backgroundColor: colors.background
  }
});
ui.root.insert(0, panel);

var titleLabel = ui.Label({
  value: 'SATELLITE INDEX EXPLORER',
  style: {
    color: colors.accent,
    backgroundColor: colors.primaryDark,
    fontWeight: 'bold',
    fontSize: '19px',
    margin: '0 0 3px 0',
    padding: '0',
    stretch: 'horizontal'
  }
});
var subtitleLabel = ui.Label({
  value: 'Surface reflectance indices and raster export',
  style: {
    color: '#dce9e1',
    backgroundColor: colors.primaryDark,
    fontSize: '12px',
    margin: '0',
    padding: '0',
    stretch: 'horizontal'
  }
});
var headerPanel = ui.Panel({
  widgets: [titleLabel, subtitleLabel],
  style: {
    backgroundColor: colors.primaryDark,
    padding: '18px 16px 16px 16px',
    stretch: 'horizontal'
  }
});
var instructionLabel = ui.Label({
  value: 'Choose a data source and date, mark your area, then select and clip an image.',
  style: {
    fontSize: '13px',
    color: colors.muted,
    backgroundColor: '#e8eee9',
    padding: '10px 12px',
    margin: '12px 14px 8px 14px',
    border: '1px solid ' + colors.border,
    borderRadius: '4px',
    whiteSpace: 'normal'
  }
});
var thumbsPanel = ui.Panel({style: {padding: '4px 12px 12px 12px'}});
var mosaicInfoLabel = ui.Label({
  value: '',
  style: {
    color: colors.muted,
    fontSize: '12px',
    margin: '0 14px 4px 14px'
  }
});
var clipPrompt = ui.Label({
  value: 'Use the map toolbar to draw a free-form polygon or rectangle.',
  style: {
    fontWeight: 'bold',
    fontSize: '13px',
    color: colors.primaryDark,
    backgroundColor: '#e8eee9',
    padding: '12px',
    margin: '14px',
    border: '1px solid ' + colors.border,
    borderRadius: '4px'
  }
});

function sectionLabel(step, text) {
  return ui.Label({
    value: step + '  ' + text.toUpperCase(),
    style: {
      color: colors.primary,
      fontWeight: 'bold',
      fontSize: '11px',
      margin: '10px 14px 5px 14px'
    }
  });
}

function statusCard() {
  return ui.Label({
    value: chosenSatellite + '  •  ' + indexOptions[chosenIndex].label +
      '  •  ' + chosenProduct,
    style: {
      color: colors.text,
      backgroundColor: colors.card,
      padding: '9px 11px',
      margin: '12px 14px 6px 14px',
      border: '1px solid ' + colors.border,
      borderRadius: '4px',
      fontSize: '12px'
    }
  });
}

var drawingTools = Map.drawingTools();
drawingTools.setShown(false);
drawingTools.setDrawModes([]);

// Handle both drawing stages without accumulating callbacks
drawingTools.onDraw(function(geometry) {
  drawingTools.layers().reset();
  if (drawingStage === 'roi') {
    roi = geometry;
    Map.layers().set(0, ui.Map.Layer(roi, {color: 'red'}, 'Selected AOI'));
    checkSelectionReady();
    return;
  }
  clipPolygon = geometry;
  showClippedIndex();
});

// Select the search area with a map click
Map.onClick(function(coordinates) {
  if (drawingStage !== 'roi') {
    return;
  }

  roi = ee.Geometry.Point([coordinates.lon, coordinates.lat]);
  Map.layers().set(0, ui.Map.Layer(roi, {color: 'red'}, 'Search location'));
  checkSelectionReady();
});

var downloadButton = ui.Button({
  label: 'Download satellite image',
  onClick: processAndDownload,
  style: {
    stretch: 'horizontal',
    color: colors.primaryDark,
    backgroundColor: '#c9ddcf',
    fontWeight: 'bold',
    margin: '10px 14px 4px 14px'
  }
});
var goBackButton = ui.Button({
  label: '← Choose a Different Month',
  onClick: function() {
    requestVersion++;
    selectedImage = null;
    selectedCollection = null;
    chosenMonth = null;
    drawingStage = 'roi';
    clipPolygon = null;
    drawingTools.layers().reset();
    drawingTools.setShown(false);
    drawingTools.setDrawModes([]);
    monthSelect.setValue(null, false);
    Map.layers().reset();

    if (roi) {
      Map.layers().set(0, ui.Map.Layer(roi, {color: 'red'}, 'Selected AOI'));
    }

    showInitialPanel();
  },
  style: {
    stretch: 'horizontal',
    color: colors.primaryDark,
    backgroundColor: '#e3e8e4',
    margin: '4px 14px 12px 14px'
  }
});

/**** SELECTORS ****/
var years = [];
for (var year = satelliteOptions[chosenSatellite].startYear;
  year <= satelliteOptions[chosenSatellite].endYear; year++) {
  years.push(year.toString());
}

var months = [
  {label: 'January', value: '01'}, {label: 'February', value: '02'},
  {label: 'March', value: '03'}, {label: 'April', value: '04'},
  {label: 'May', value: '05'}, {label: 'June', value: '06'},
  {label: 'July', value: '07'}, {label: 'August', value: '08'},
  {label: 'September', value: '09'}, {label: 'October', value: '10'},
  {label: 'November', value: '11'}, {label: 'December', value: '12'}
];

var satelliteSelect = ui.Select({
  items: Object.keys(satelliteOptions),
  value: chosenSatellite,
  style: {
    stretch: 'horizontal',
    margin: '0 14px 2px 14px',
    backgroundColor: colors.card
  },
  onChange: function(value) {
    requestVersion++;
    chosenSatellite = value;
    chosenYear = null;
    chosenMonth = null;
    selectedImage = null;
    selectedCollection = null;
    clipPolygon = null;
    drawingStage = 'roi';
    drawingTools.layers().reset();
    drawingTools.setShown(false);
    drawingTools.setDrawModes([]);
    updateYearOptions();
    monthSelect.setValue(null, false);
    updateIndexOptions();
    showInitialPanel();
  }
});

var indexSelect = ui.Select({
  items: indexLabels,
  value: indexOptions.GPI.label,
  style: {
    stretch: 'horizontal',
    margin: '0 14px 2px 14px',
    backgroundColor: colors.card
  },
  onChange: function(label) {
    chosenIndex = indexNames[indexLabels.indexOf(label)];
    if (selectedImage && clipPolygon) {
      showClippedIndex();
    } else if (selectedImage) {
      showSelectedImage(selectedImage);
    }
  }
});
var productSelect = ui.Select({
  items: ['Single Scene (LEAST CLOUDY)', 'Monthly Median Mosaic'],
  value: chosenProduct,
  style: {
    stretch: 'horizontal',
    margin: '0 14px 2px 14px',
    backgroundColor: colors.card
  },
  onChange: function(value) {
    requestVersion++;
    chosenProduct = value;
    selectedImage = null;
    selectedCollection = null;
    clipPolygon = null;
    drawingStage = 'roi';
    drawingTools.layers().reset();
    drawingTools.setShown(false);
    drawingTools.setDrawModes([]);
    checkSelectionReady();
  }
});
var yearSelect = ui.Select({
  items: years,
  placeholder: 'Choose Year',
  style: {stretch: 'horizontal', margin: '0 4px 0 0', backgroundColor: colors.card},
  onChange: function(value) {
    chosenYear = value;
    checkSelectionReady();
  }
});
var monthSelect = ui.Select({
  items: months.map(function(month) { return month.label; }),
  placeholder: 'Choose Month',
  style: {stretch: 'horizontal', margin: '0 0 0 4px', backgroundColor: colors.card},
  onChange: function(label) {
    chosenMonth = months[monthSelect.items().indexOf(label)].value;
    checkSelectionReady();
  }
});

// Reuse the container so its controls retain a single parent
var datePanel = ui.Panel({
  widgets: [yearSelect, monthSelect],
  layout: ui.Panel.Layout.flow('horizontal'),
  style: {stretch: 'horizontal', margin: '0 14px 4px 14px'}
});

function updateYearOptions() {
  var satellite = satelliteOptions[chosenSatellite];
  years = [];
  for (var year = satellite.startYear; year <= satellite.endYear; year++) {
    years.push(year.toString());
  }
  yearSelect.items().reset(years);
  yearSelect.setValue(null, false);
}

function updateIndexOptions() {
  indexNames = satelliteOptions[chosenSatellite].supportsGPI
    ? Object.keys(indexOptions).filter(function(name) { return name !== 'SWIR'; })
    : Object.keys(indexOptions).filter(function(name) { return name !== 'GPI'; });
  indexLabels = indexNames.map(function(name) { return indexOptions[name].label; });

  if (indexNames.indexOf(chosenIndex) === -1) {
    chosenIndex = 'NDVI';
  }
  indexSelect.items().reset(indexLabels);
  indexSelect.setValue(indexOptions[chosenIndex].label, false);
}

function resetApp() {
  requestVersion++;
  roi = null;
  selectedImage = null;
  selectedCollection = null;
  clipPolygon = null;
  chosenYear = null;
  chosenMonth = null;
  chosenSatellite = 'Sentinel-2';
  chosenIndex = 'GPI';
  chosenProduct = 'Single Scene (LEAST CLOUDY)';
  drawingStage = 'roi';

  satelliteSelect.setValue(chosenSatellite, false);
  productSelect.setValue(chosenProduct, false);
  updateYearOptions();
  monthSelect.setValue(null, false);
  updateIndexOptions();
  drawingTools.layers().reset();
  drawingTools.setShown(false);
  drawingTools.setDrawModes([]);
  Map.layers().reset();
  Map.setCenter(10, 50, 4);
  showInitialPanel();
}

function makeResetButton() {
  return ui.Button({
    label: '↻ Reset App',
    onClick: resetApp,
    style: {
      stretch: 'horizontal',
      color: colors.primaryDark,
      backgroundColor: '#edf1ee',
      fontWeight: 'bold',
      margin: '4px 14px 14px 14px',
      border: '1px solid ' + colors.border
    }
  });
}

function showInitialPanel() {
  panel.clear();
  panel.add(headerPanel);
  panel.add(instructionLabel);
  panel.add(sectionLabel('01', 'Satellite'));
  panel.add(satelliteSelect);
  panel.add(sectionLabel('02', 'Spectral product'));
  panel.add(indexSelect);
  panel.add(sectionLabel('03', 'Acquisition date'));
  panel.add(datePanel);
  panel.add(sectionLabel('04', 'Image product'));
  panel.add(productSelect);
  panel.add(sectionLabel('05', 'Area of interest'));
  panel.add(ui.Label({
    value: 'Click anywhere inside your area on the map. No drawing tool is needed for this step.',
    style: {
      color: colors.text,
      backgroundColor: colors.card,
      padding: '10px 12px',
      margin: '0 14px 16px 14px',
      border: '1px solid ' + colors.border,
      borderRadius: '4px',
      fontSize: '12px'
    }
  }));
  panel.add(makeResetButton());
}

showInitialPanel();

function checkSelectionReady() {
  if (chosenYear && chosenMonth && roi) {
    if (chosenProduct === 'Monthly Median Mosaic') {
      createMonthlyMedian(chosenYear, chosenMonth);
    } else {
      updateThumbnails(chosenYear, chosenMonth);
    }
  }
}

/**** IMAGE SELECTION AND MOSAICS ****/
function createMonthlyMedian(selectedYear, selectedMonth) {
  var currentRequest = ++requestVersion;
  selectedImage = null;
  selectedCollection = null;
  clipPolygon = null;

  panel.clear();
  panel.add(headerPanel);
  panel.add(statusCard());
  panel.add(ui.Label({
    value: 'Building cloud-masked monthly median…',
    style: {
      color: colors.primary,
      backgroundColor: colors.card,
      padding: '12px',
      margin: '12px 14px',
      border: '1px solid ' + colors.border,
      borderRadius: '4px'
    }
  }));
  panel.add(makeResetButton());

  var monthlyImages = getMonthlyCollection(selectedYear, selectedMonth);
  monthlyImages.size().evaluate(function(sceneCount) {
    if (currentRequest !== requestVersion) {
      return;
    }

    if (!sceneCount) {
      panel.clear();
      panel.add(headerPanel);
      panel.add(statusCard());
      panel.add(ui.Label({
        value: 'No scenes were found for this area and month.',
        style: {
          color: '#8a3d2f',
          backgroundColor: '#f8e9e5',
          padding: '11px',
          margin: '12px 14px',
          border: '1px solid #e7c9c1',
          borderRadius: '4px'
        }
      }));
      panel.add(goBackButton);
      panel.add(makeResetButton());
      return;
    }

    selectedCollection = monthlyImages.map(function(image) {
      return prepareImage(image, true);
    });
    selectedImage = selectedCollection.median();
    mosaicInfoLabel.setValue(
      sceneCount + ' cloud-masked scene' + (sceneCount === 1 ? '' : 's') +
      ' included in the monthly median.'
    );
    drawingStage = 'clip';
    drawingTools.layers().reset();
    drawingTools.setShown(true);
    drawingTools.setDrawModes(['polygon', 'rectangle']);
    showSelectedImage(selectedImage);
    Map.centerObject(roi, 12);
  });
}

function updateThumbnails(selectedYear, selectedMonth) {
  selectedImage = null;
  selectedCollection = null;
  drawingStage = 'roi';
  drawingTools.setShown(false);
  drawingTools.setDrawModes([]);
  panel.clear();
  panel.add(headerPanel);
  panel.add(statusCard());
  panel.add(sectionLabel('05', 'Select a scene'));
  panel.add(thumbsPanel);
  panel.add(makeResetButton());
  thumbsPanel.clear();

  var satellite = satelliteOptions[chosenSatellite];
  var filtered = getMonthlyCollection(selectedYear, selectedMonth)
    .sort(satellite.cloudProperty)
    .limit(3);

  filtered.toList(3).evaluate(function(imageList) {
    if (!imageList || imageList.length === 0) {
      thumbsPanel.add(ui.Label({
        value: 'No scenes were found for this area and month.',
        style: {
          color: '#8a3d2f',
          backgroundColor: '#f8e9e5',
          padding: '11px',
          margin: '5px 2px 10px 2px',
          border: '1px solid #e7c9c1',
          borderRadius: '4px'
        }
      }));
      thumbsPanel.add(goBackButton);
      return;
    }

    imageList.forEach(function(imageInfo) {
      var image = prepareImage(ee.Image(imageInfo.id));
      var properties = imageInfo.properties;
      var date = new Date(properties['system:time_start']).toISOString().split('T')[0];
      var cloudValue = properties[satellite.cloudProperty];
      var cloudText = cloudValue === undefined
        ? 'Clouds: N/A'
        : 'Clouds: ' + cloudValue.toFixed(1) + '%';
      var thumbnail = ui.Thumbnail({
        image: image,
        params: {
          dimensions: '150x150',
          bands: ['red', 'green', 'blue'],
          min: 0,
          max: 0.3
        },
        style: {stretch: 'horizontal', margin: '0 0 4px 0'}
      });
      var metadata = ui.Label({
        value: 'Date: ' + date + '\n' + cloudText,
        style: {whiteSpace: 'pre', fontSize: '12px', margin: '4px 0'}
      });
      var selectButton = ui.Button({
        label: 'Select Image',
        onClick: function() {
          selectedImage = image;
          clipPolygon = null;
          drawingStage = 'clip';
          drawingTools.layers().reset();
          drawingTools.setShown(true);
          drawingTools.setDrawModes(['polygon', 'rectangle']);
          showSelectedImage(image);
          Map.centerObject(roi, 12);
        }
      });
      selectButton.style().set({
        stretch: 'horizontal',
        color: colors.primaryDark,
        backgroundColor: '#c9ddcf',
        fontWeight: 'bold',
        margin: '6px 0 0 0'
      });
      thumbsPanel.add(ui.Panel({
        widgets: [thumbnail, metadata, selectButton],
        layout: ui.Panel.Layout.flow('vertical'),
        style: {
          stretch: 'horizontal',
          backgroundColor: colors.card,
          padding: '10px',
          margin: '5px 2px',
          border: '1px solid ' + colors.border,
          borderRadius: '5px'
        }
      }));
    });
    thumbsPanel.add(goBackButton);
  });
}

function showSelectedImage(image) {
  Map.layers().reset();
  Map.addLayer(image, rgbViz, 'RGB');
  Map.addLayer(
    getSelectedIndexImage(),
    indexOptions[chosenIndex].viz,
    chosenIndex
  );
  panel.clear();
  panel.add(headerPanel);
  panel.add(statusCard());
  panel.add(sectionLabel('06', chosenProduct === 'Monthly Median Mosaic'
    ? 'Monthly median result'
    : 'Displayed product'));
  if (chosenProduct === 'Monthly Median Mosaic') {
    panel.add(mosaicInfoLabel);
  }
  panel.add(indexSelect);
  panel.add(clipPolygon ? downloadButton : clipPrompt);
  panel.add(goBackButton);
  panel.add(makeResetButton());
}

function showClippedIndex() {
  var clippedIndex = getSelectedIndexImage().clip(clipPolygon);
  Map.layers().reset();
  Map.addLayer(
    clippedIndex,
    indexOptions[chosenIndex].viz,
    'Clipped ' + chosenIndex
  );
  panel.clear();
  panel.add(headerPanel);
  panel.add(statusCard());
  panel.add(sectionLabel('07', 'Review and download'));
  panel.add(indexSelect);
  panel.add(downloadButton);
  panel.add(goBackButton);
  panel.add(makeResetButton());
}

/**** DOWNLOAD ****/
function processAndDownload() {
  if (!selectedImage || !clipPolygon) {
    panel.add(ui.Label({
      value: 'Select an image and draw a clipping boundary first.',
      style: {
        color: '#8a3d2f',
        backgroundColor: '#f8e9e5',
        padding: '10px',
        margin: '8px 14px',
        border: '1px solid #e7c9c1'
      }
    }));
    return;
  }

  var clippedIndex = getSelectedIndexImage().clip(clipPolygon);
  var productName = chosenProduct === 'Monthly Median Mosaic'
    ? 'Monthly_Median'
    : 'Single_Scene';
  var downloadParameters = {
    name: chosenSatellite.replace(' ', '_') + '_' + chosenIndex + '_' +
      chosenYear + '_' + chosenMonth + '_' + productName,
    scale: satelliteOptions[chosenSatellite].scale,
    region: clipPolygon,
    format: 'GEO_TIFF',
    filePerBand: false
  };

  panel.clear();
  panel.add(headerPanel);
  panel.add(statusCard());
  panel.add(sectionLabel('08', 'Preparing export'));
  panel.add(ui.Label({
    value: 'Earth Engine is creating the download link. This can take up to a minute.',
    style: {
      color: colors.text,
      backgroundColor: colors.card,
      padding: '11px',
      margin: '0 14px 6px 14px',
      border: '1px solid ' + colors.border,
      borderRadius: '4px'
    }
  }));

  clippedIndex.getDownloadURL(downloadParameters, function(downloadUrl, error) {
    panel.clear();
    panel.add(headerPanel);
    panel.add(statusCard());

    if (error || !downloadUrl) {
      panel.add(sectionLabel('08', 'Export failed'));
      panel.add(ui.Label({
        value: error || 'Earth Engine could not create the download link. Try a smaller clipping area.',
        style: {
          color: '#8a3d2f',
          backgroundColor: '#f8e9e5',
          padding: '11px',
          margin: '0 14px 8px 14px',
          border: '1px solid #e7c9c1',
          borderRadius: '4px'
        }
      }));
    } else {
      panel.add(sectionLabel('08', 'Export ready'));
      panel.add(ui.Label({
        value: 'Your clipped ' + chosenIndex + ' GeoTIFF is ready.',
        style: {
          color: colors.text,
          backgroundColor: colors.card,
          padding: '11px',
          margin: '0 14px 6px 14px',
          border: '1px solid ' + colors.border,
          borderRadius: '4px'
        }
      }));
      panel.add(ui.Label({
        value: '↓  Download satellite image',
        style: {
          color: colors.primaryDark,
          backgroundColor: colors.accent,
          fontWeight: 'bold',
          textAlign: 'center',
          padding: '11px',
          margin: '4px 14px 8px 14px',
          borderRadius: '4px',
          stretch: 'horizontal'
        },
        targetUrl: downloadUrl
      }));
    }

    panel.add(goBackButton);
    panel.add(makeResetButton());
  });
}
