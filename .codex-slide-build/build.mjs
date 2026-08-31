import fs from "node:fs/promises";
import { Presentation, PresentationFile } from "@oai/artifact-tool";

const outputPath = "/Users/taylorcraft/Documents/Godwit_toolkit/Satellite_Index_Explorer.pptx";
const previewPath = "/Users/taylorcraft/Documents/Godwit_toolkit/.codex-slide-build/slide-1.png";
const screenshotPath = "/var/folders/d5/hr6nhftx5wbcp7x2sd8h9frw0000gn/T/codex-clipboard-c6a7e346-c895-4ea6-909c-9820a1e1f045.png";
const screenshotBytes = await fs.readFile(screenshotPath);

const presentation = Presentation.create({ slideSize: { width: 1280, height: 720 } });
const slide = presentation.slides.add();
slide.background.fill = "#F7F7F3";

// title block
const eyebrow = slide.shapes.add({
  geometry: "textbox",
  name: "eyebrow",
  position: { left: 62, top: 44, width: 420, height: 28 },
  fill: "none",
  line: { style: "solid", fill: "none", width: 0 },
});
eyebrow.text = "GOOGLE EARTH ENGINE APP";
eyebrow.text.style = { fontSize: 15, bold: true, color: "#D39A2C", typeface: "Helvetica Neue" };

const title = slide.shapes.add({
  geometry: "textbox",
  name: "title",
  position: { left: 62, top: 82, width: 1120, height: 64 },
  fill: "none",
  line: { style: "solid", fill: "none", width: 0 },
});
title.text = "Satellite Index Explorer";
title.text.style = { fontSize: 44, bold: true, color: "#153E2C", typeface: "Helvetica Neue" };

const subtitle = slide.shapes.add({
  geometry: "textbox",
  name: "subtitle",
  position: { left: 62, top: 143, width: 670, height: 42 },
  fill: "none",
  line: { style: "solid", fill: "none", width: 0 },
});
subtitle.text = "Explore surface reflectance indices and export analysis-ready rasters";
subtitle.text.style = { fontSize: 22, color: "#56645B", typeface: "Helvetica Neue" };

// feature summary
const bulletItems = [
  ["MULTI-SENSOR", "Sentinel-2 and Landsat 7, 8 & 9 imagery"],
  ["SEVEN INDICES", "GPI, NDVI, EVI, NDWI, NDMI, SAVI and SWIR"],
  ["FLEXIBLE COMPOSITES", "Least-cloudy scenes or monthly median mosaics"],
  ["READY TO USE", "Interactive clipping and native-resolution GeoTIFF export"],
];

bulletItems.forEach(([label, body], index) => {
  const top = 228 + index * 92;
  const marker = slide.shapes.add({
    geometry: "ellipse",
    name: `marker-${index + 1}`,
    position: { left: 64, top: top + 5, width: 14, height: 14 },
    fill: index === 1 ? "#D39A2C" : "#24543D",
    line: { style: "solid", fill: "none", width: 0 },
  });
  const heading = slide.shapes.add({
    geometry: "textbox",
    name: `feature-heading-${index + 1}`,
    position: { left: 94, top, width: 450, height: 25 },
    fill: "none",
    line: { style: "solid", fill: "none", width: 0 },
  });
  heading.text = label;
  heading.text.style = { fontSize: 16, bold: true, color: "#153E2C", typeface: "Helvetica Neue" };

  const description = slide.shapes.add({
    geometry: "textbox",
    name: `feature-description-${index + 1}`,
    position: { left: 94, top: top + 29, width: 470, height: 46 },
    fill: "none",
    line: { style: "solid", fill: "none", width: 0 },
  });
  description.text = body;
  description.text.style = { fontSize: 20, color: "#303B34", typeface: "Helvetica Neue" };
});

// screenshot evidence
slide.shapes.add({
  geometry: "rect",
  name: "image-accent",
  position: { left: 607, top: 205, width: 595, height: 428 },
  fill: "#D8A441",
  line: { style: "solid", fill: "none", width: 0 },
});

slide.images.add({
  blob: screenshotBytes,
  contentType: "image/png",
  alt: "Satellite Index Explorer showing a Landsat 9 monthly median surface-water index over southern Spain",
  fit: "cover",
  crop: { left: 0, top: 0.17, right: 0.35, bottom: 0.22 },
  position: { left: 596, top: 194, width: 595, height: 428 },
});

const caption = slide.shapes.add({
  geometry: "textbox",
  name: "image-caption",
  position: { left: 596, top: 638, width: 595, height: 26 },
  fill: "none",
  line: { style: "solid", fill: "none", width: 0 },
});
caption.text = "Example: Landsat 9 SWIR monthly median mosaic over southern Spain";
caption.text.style = { fontSize: 14, color: "#6B746D", italic: true, typeface: "Helvetica Neue" };

slide.speakerNotes.textFrame.setText(
  "[Sources]\n- User-provided Satellite Index Explorer screenshot.\n- Project source: GEE_code/GPI Raster Export.js."
);

const preview = await presentation.export({ slide, format: "png", scale: 2 });
await fs.writeFile(previewPath, new Uint8Array(await preview.arrayBuffer()));
const layout = await slide.export({ format: "layout" });
await fs.writeFile(
  "/Users/taylorcraft/Documents/Godwit_toolkit/.codex-slide-build/slide-1.layout.json",
  await layout.text(),
);
const pptx = await PresentationFile.exportPptx(presentation);
await pptx.save(outputPath);
