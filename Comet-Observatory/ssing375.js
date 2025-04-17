// Set up dimensions and margins
const width = 960;
const height = 700;
const margin = { top: 50, right: 50, bottom: 50, left: 50 };
const innerWidth = width - margin.left - margin.right;
const innerHeight = height - margin.top - margin.bottom;
const centerX = innerWidth / 2;
const centerY = innerHeight / 2;

// Sound variables
let soundEnabled = false;
let synth = null;
let lastPlayedComet = null;

// Create SVG
const svg = d3.select("#vis")
  .append("svg")
  .attr("width", width)
  .attr("height", height);

// Add a sound toggle button if it doesn't exist
if (!document.getElementById("soundToggle")) {
  const soundToggle = document.createElement("button");
  soundToggle.id = "soundToggle";
  soundToggle.className = "feature-button";
  soundToggle.textContent = "🔊 Sound Off";
  
  // Insert the button in the controls area
  const controlsDiv = document.getElementById("controls");
  if (controlsDiv) {
    controlsDiv.appendChild(soundToggle);
  } else {
    document.body.appendChild(soundToggle);
  }
}

// Add a glow filter for comets
function addGlowFilter(svg) {
  const defs = svg.append("defs");
  const filter = defs.append("filter")
    .attr("id", "glow");

  filter.append("feGaussianBlur")
    .attr("stdDeviation", "2.5")
    .attr("result", "coloredBlur");

  const feMerge = filter.append("feMerge");
  feMerge.append("feMergeNode")
    .attr("in", "coloredBlur");
  feMerge.append("feMergeNode")
    .attr("in", "SourceGraphic");
}

addGlowFilter(svg);

// Add a starry background
const starsGroup = svg.append("g").attr("class", "stars");
for (let i = 0; i < 200; i++) {
  const x = Math.random() * width;
  const y = Math.random() * height;
  const r = Math.random() * 1.5;
  const opacity = Math.random() * 0.8 + 0.2;
  
  starsGroup.append("circle")
    .attr("cx", x)
    .attr("cy", y)
    .attr("r", r)
    .attr("fill", "#FFFFFF")
    .attr("opacity", opacity);
}

// Create main visualization group
const g = svg.append("g")
  .attr("transform", `translate(${margin.left + centerX}, ${margin.top + centerY})`);

// Add Sun at the center
g.append("circle")
  .attr("class", "sun")
  .attr("cx", 0)
  .attr("cy", 0)
  .attr("r", 15)
  .attr("fill", "#FDB813")
  .attr("stroke", "#F89406")
  .attr("stroke-width", 2);

// Add Earth orbit (reference)
const earthOrbitRadius = 120;
g.append("circle")
  .attr("class", "earth-orbit")
  .attr("cx", 0)
  .attr("cy", 0)
  .attr("r", earthOrbitRadius)
  .attr("fill", "none")
  .attr("stroke", "#2E75B6")
  .attr("stroke-width", 1)
  .attr("stroke-dasharray", "5,5")
  .attr("opacity", 0.6);

// Add Earth
const earth = g.append("circle")
  .attr("class", "earth")
  .attr("cx", earthOrbitRadius)
  .attr("cy", 0)
  .attr("r", 8)
  .attr("fill", "#2E75B6")
  .attr("stroke", "#1E5799")
  .attr("stroke-width", 1);

// Earth label
g.append("text")
  .attr("class", "annotation")
  .attr("x", earthOrbitRadius + 12)
  .attr("y", 5)
  .attr("text-anchor", "start")
  .attr("fill", "#FFFFFF")
  .text("Earth");

// Create tooltip div - Ensure it exists and has proper styles
let tooltip = d3.select("body").select("#tooltip");
if (tooltip.empty()) {
  tooltip = d3.select("body").append("div")
    .attr("id", "tooltip")
    .style("opacity", 0)
    .style("position", "absolute")
    .style("background", "rgba(0, 0, 0, 0.8)")
    .style("color", "white")
    .style("padding", "10px")
    .style("border-radius", "5px")
    .style("pointer-events", "none")
    .style("z-index", "1000")
    .style("box-shadow", "0 0 10px rgba(0,0,0,0.5)")
    .style("max-width", "300px");
}

// Initialize animation variables
let currentTime = 0;
let animationRunning = false;
let timeScale = 0.01; // Controls animation speed
let comets = []; // Will store comet objects for animation

// Initialize global variables for filtering
let selectedPeriodCategory = "all";
let selectedInclinationCategory = "all";
let allOrbits = [];
let allComets = [];

// Function to initialize audio system
function initializeAudio() {
  try {
    // Make sure Tone.js is loaded
    if (typeof Tone === 'undefined') {
      console.error("Tone.js is not loaded. Including it dynamically.");
      
      // Dynamically add Tone.js if it's not available
      const toneScript = document.createElement('script');
      toneScript.src = "https://cdnjs.cloudflare.com/ajax/libs/tone/14.8.49/Tone.min.js";
      toneScript.onload = function() {
        console.log("Tone.js loaded successfully");
        setupToneSynth();
      };
      document.head.appendChild(toneScript);
      return;
    }
    
    setupToneSynth();
  } catch (error) {
    console.error("Error initializing audio system:", error);
    soundEnabled = false;
  }
}

// Separate function to create the synth
function setupToneSynth() {
  try {
    // Create a polyphonic synth
    synth = new Tone.PolySynth(Tone.Synth).toDestination();
    
    // Set initial volume (quieter by default)
    Tone.Destination.volume.value = -10;
    
    console.log("Audio synth initialized successfully");
    
    // Set up sound toggle functionality
    const soundToggle = document.getElementById("soundToggle");
    if (soundToggle) {
      soundToggle.addEventListener("click", function() {
        soundEnabled = !soundEnabled;
        this.textContent = soundEnabled ? "🔊 Sound On" : "🔊 Sound Off";
        this.classList.toggle("active", soundEnabled);
        
        // Start audio context when sound is enabled
        if (soundEnabled) {
          Tone.start().then(() => {
            console.log("Tone.js context started");
            // Play a test sound
            synth.triggerAttackRelease("C4", 0.3);
          }).catch(err => {
            console.error("Could not start Tone.js:", err);
          });
        }
      });
    }
  } catch (error) {
    console.error("Error setting up Tone synth:", error);
  }
}

// Function to play sound based on comet properties
function playCometSound(comet) {
  if (!soundEnabled || !synth) return;
  
  try {
    // Ensure Tone.js is started
    if (Tone.context && Tone.context.state !== 'running') {
      Tone.context.resume();
    }
    
    // Don't play the same comet repeatedly too quickly
    if (lastPlayedComet === comet.name) return;
    lastPlayedComet = comet.name;
    
    // Calculate pitch based on period (lower period = higher pitch)
    // Map period to a musical scale
    const period = comet.period || 10;
    const baseNote = 60; // Middle C in MIDI
    const noteOffset = Math.round(12 * (1 - Math.min(period, 100) / 100));
    const midiNote = baseNote + noteOffset;
    
    // Convert MIDI to frequency
    const freq = Tone.Frequency(midiNote, "midi");
    
    // Calculate duration based on eccentricity (more eccentric = shorter)
    const duration = 0.2 + (1 - (comet.e || 0.5)) * 0.8;
    
    // Calculate volume based on perihelion (closer = louder)
    const volume = -15 + (1 / (comet.q || 1)) * 5;
    
    // Play the sound
    synth.triggerAttackRelease(freq, duration, Tone.now(), 0.7);
    
    console.log(`Playing sound for comet ${comet.name}, frequency: ${freq.toFrequency()}`);
    
    // Reset after a timeout
    setTimeout(() => {
      if (lastPlayedComet === comet.name) {
        lastPlayedComet = null;
      }
    }, 1000);
  } catch (error) {
    console.error("Error playing comet sound:", error);
  }
}

// Mythological data for selected comets
const mythologyData = {
  "1P/Halley": {
    title: "Halley's Comet: Herald of Omens",
    content: `
      <p>Halley's Comet has been observed since at least 240 BCE and has been linked to numerous historical events:</p>
      <ul>
        <li>In 66 CE, it appeared over Jerusalem just before the First Jewish-Roman War</li>
        <li>In 1066, it was seen as an omen before the Battle of Hastings and is depicted in the Bayeux Tapestry</li>
        <li>In 1910, its passage caused panic due to (incorrect) reports that its tail contained deadly gas</li>
      </ul>
      <p>Its most recent appearance was in 1986, and it will return in 2061.</p>
    `,
    imageUrl: "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Halley_bayeux.jpg/320px-Halley_bayeux.jpg"
  },
  "109P/Swift-Tuttle": {
    title: "Swift-Tuttle: The Doomsday Comet",
    content: `
      <p>This comet is the parent body of the Perseid meteor shower, one of the most spectacular annual meteor displays.</p>
      <p>Sometimes called "The Doomsday Comet," it has the potential to be catastrophic to Earth due to its size and orbit. 
      Astronomer Gerrit L. Verschuur described it as "the single most dangerous object known to humanity."</p>
      <p>Its next approach in 2126 will bring it within 14 million miles of Earth.</p>
    `,
    imageUrl: "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3f/Perseid.jpg/320px-Perseid.jpg"
  },
  "2P/Encke": {
    title: "Encke's Comet: The First Periodic Comet",
    content: `
      <p>Encke's Comet has the shortest known orbital period for a comet at 3.3 years, and was the first periodic comet discovered after Halley's Comet.</p>
      <p>In ancient Greek mythology, some astronomers believe Encke may have been the comet that the philosopher Aristotle observed in 373 BCE, which he described as "shaped like a road."</p>
      <p>It is also associated with the Taurid meteor stream, which some researchers link to ancient catastrophes and the development of early human fear of comets.</p>
    `,
    imageUrl: "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/2P-Encke.jpg/320px-2P-Encke.jpg"
  },
  "96P/Machholz 1": {
    title: "Machholz: The Unusual Visitor",
    content: `
      <p>This comet has an unusual composition that has led some astronomers to suggest it might have originated from outside our solar system.</p>
      <p>It is associated with several meteor showers, including the Arietids, Southern Delta Aquariids, and Northern Delta Aquariids.</p>
      <p>In some cultures, strong meteor showers were seen as celestial tears or spirits returning to the heavens.</p>
    `,
    imageUrl: "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Machholz.jpg/320px-Machholz.jpg"
  }
};

// Timeline data for selected comets
function generateTimelineData(cometName) {
  // Common template for timeline events
  const commonEvents = [
    { year: "Future", event: "Projected next appearance in the inner Solar System" },
    { year: "Present", event: "Included in the NASA Near Earth Comets database" }
  ];
  
  // Specific data for well-known comets
  const specificData = {
    "1P/Halley": [
      { year: "1986", event: "Last observed passage, studied by the Giotto and Vega spacecraft" },
      { year: "1910", event: "Caused public panic due to Earth's passage through its tail" },
      { year: "1066", event: "Depicted in the Bayeux Tapestry, considered an omen for the Battle of Hastings" },
      { year: "240 BCE", event: "First confirmed recorded observation by Chinese astronomers" }
    ],
    "2P/Encke": [
      { year: "2013", event: "Observed by STEREO spacecraft during its latest return" },
      { year: "1819", event: "Johann Franz Encke computed its orbit and predicted its return" },
      { year: "1786", event: "First observed by Pierre Méchain, but not recognized as periodic" }
    ],
    "109P/Swift-Tuttle": [
      { year: "1992", event: "Last observed passage near Earth" },
      { year: "1865", event: "Rediscovered independently by Horace Tuttle" },
      { year: "1862", event: "Discovered by Lewis Swift and later by Horace Tuttle" }
    ],
    "67P/Churyumov-Gerasimenko": [
      { year: "2014-2016", event: "Studied extensively by the Rosetta spacecraft and Philae lander" },
      { year: "1969", event: "Discovered by Klim Churyumov on a photograph taken by Svetlana Gerasimenko" }
    ]
  };
  
  // If we have specific data for this comet, use it
  if (specificData[cometName]) {
    return [...specificData[cometName], ...commonEvents];
  }
  
  // Generate generic timeline for other comets
  return [
    { year: "Recent", event: "Observed and tracked by modern astronomical equipment" },
    { year: "Discovery", event: "First identified and cataloged as " + cometName },
    ...commonEvents
  ];
}

// Function to show the timeline panel
function showTimelinePanel(comet) {
  const timelineData = generateTimelineData(comet.name);
  const timelineContent = document.getElementById("timelineContent");
  
  // Clear previous content
  timelineContent.innerHTML = "";
  
  // Add comet name as header
  const header = document.createElement("h4");
  header.textContent = comet.name;
  timelineContent.appendChild(header);
  
  // Add timeline events
  timelineData.forEach(item => {
    const eventDiv = document.createElement("div");
    eventDiv.className = "timeline-event";
    
    const yearSpan = document.createElement("span");
    yearSpan.className = "timeline-year";
    yearSpan.textContent = item.year;
    
    const eventText = document.createElement("p");
    eventText.textContent = item.event;
    
    eventDiv.appendChild(yearSpan);
    eventDiv.appendChild(eventText);
    timelineContent.appendChild(eventDiv);
  });
  
  // Add sound note if sound is enabled
  if (soundEnabled) {
    const soundNote = document.createElement("p");
    soundNote.className = "sound-note";
    soundNote.textContent = `The musical tone you hear represents this comet's ${comet.period.toFixed(1)} year orbital period.`;
    timelineContent.appendChild(soundNote);
  }
  
  // Show the panel
  document.getElementById("timelinePanel").style.display = "block";
}

// Function to show the mythology panel
function showMythologyPanel(cometName) {
  // Check if we have mythology data for this comet
  if (!mythologyData[cometName]) return;
  
  const data = mythologyData[cometName];
  const content = document.getElementById("mythologyContent");
  
  // Clear previous content
  content.innerHTML = "";
  
  // Add title
  const title = document.createElement("h4");
  title.textContent = data.title;
  content.appendChild(title);
  
  // Add content
  const contentDiv = document.createElement("div");
  contentDiv.className = "mythology-content";
  contentDiv.innerHTML = data.content;
  content.appendChild(contentDiv);
  
  // Add image if available
  if (data.imageUrl) {
    const img = document.createElement("img");
    img.className = "mythology-image";
    img.src = data.imageUrl;
    img.alt = `Historical depiction related to ${cometName}`;
    content.appendChild(img);
  }
  
  // Show the panel
  document.getElementById("mythologyPanel").style.display = "block";
}

// Create time controls
const timeSlider = d3.select("#timeSlider");
if (timeSlider.empty()) {
  console.warn("Time slider element not found. Creating one.");
  d3.select("#controls").append("input")
    .attr("id", "timeSlider")
    .attr("type", "range")
    .attr("min", 0)
    .attr("max", 100)
    .attr("value", 0)
    .style("width", "300px");
}

const playButton = d3.select("#playButton");
if (playButton.empty()) {
  console.warn("Play button element not found. Creating one.");
  d3.select("#controls").append("button")
    .attr("id", "playButton")
    .text("Play")
    .style("margin", "0 10px");
}

// Add year display element after the slider in the controls
if (!document.getElementById("yearDisplay")) {
  console.warn("Year display element not found. Creating one.");
  d3.select("#controls").append("span")
    .attr("id", "yearDisplay")
    .style("margin-left", "15px")
    .style("color", "white")
    .style("font-weight", "bold")
    .style("font-size", "16px")
    .text("Year: 2023");
}

// Load and process the comet data
console.log("Attempting to load comet data...");
d3.csv("data/near-earth-comets.csv").then(data => {
  console.log("CSV data loaded successfully!", data.length, "records found");
  
  // Parse and transform data
  data.forEach(d => {
    // Parse numeric attributes based on actual dataset fields
    d.e = +d.e; // eccentricity
    d.i = +d.i; // inclination
    d.q = +d.q; // perihelion distance
    d.Q = +d.Q; // aphelion distance
    d.P = +d.P; // orbital period
    
    // Handle MOID field which might have different column names
    if (d.MOID !== undefined) {
      d.MOID = +d.MOID;
    } else if (d["MOID (AU)"] !== undefined) {
      d.MOID = +d["MOID (AU)"];
    } else {
      d.MOID = 1.0; // Default value if missing
    }
    
    // Use Object_name if available, otherwise use Object
    d.name = d.Object_name || d.Object;
    
    // Add derived attributes
    d.semi_major_axis = (d.q + d.Q) / 2;
    d.semi_minor_axis = d.semi_major_axis * Math.sqrt(1 - (d.e * d.e));
  });

  // Log data sample to verify correct parsing
  console.log("Data sample:", data.slice(0, 3));

  visualizeData(data);
}).catch(error => {
  console.error("Error loading CSV data:", error);
  console.log("Current working directory might be incorrect.");
  console.log("Trying alternative paths...");
  
  // Try alternative paths
  d3.csv("./data/near-earth-comets.csv")
    .then(handleData)
    .catch(err => {
      console.error("Alternative path #1 failed:", err);
      d3.csv("../data/near-earth-comets.csv")
        .then(handleData)
        .catch(err => {
          console.error("Alternative path #2 failed:", err);
          d3.csv("/data/near-earth-comets.csv")
            .then(handleData)
            .catch(finalError => {
              console.error("All path attempts failed. Final error:", finalError);
              displayErrorMessage();
            });
        });
    });
});

// Helper function to handle data once loaded
function handleData(data) {
  console.log("CSV data loaded successfully with alternative path!", data.length, "records found");
  
  // Parse and transform data
  data.forEach(d => {
    // Parse numeric attributes based on actual dataset fields
    d.e = +d.e; // eccentricity
    d.i = +d.i; // inclination
    d.q = +d.q; // perihelion distance
    d.Q = +d.Q; // aphelion distance
    d.P = +d.P; // orbital period
    
    // Handle MOID field which might have different column names
    if (d.MOID !== undefined) {
      d.MOID = +d.MOID;
    } else if (d["MOID (AU)"] !== undefined) {
      d.MOID = +d["MOID (AU)"];
    } else {
      d.MOID = 1.0; // Default value if missing
    }
    
    // Use Object_name if available, otherwise use Object
    d.name = d.Object_name || d.Object;
    
    // Add derived attributes
    d.semi_major_axis = (d.q + d.Q) / 2;
    d.semi_minor_axis = d.semi_major_axis * Math.sqrt(1 - (d.e * d.e));
  });

  visualizeData(data);
}

// Function to display error message
function displayErrorMessage() {
  d3.select("#vis")
    .append("div")
    .attr("class", "error-message")
    .style("color", "red")
    .style("font-weight", "bold")
    .style("margin-top", "20px")
    .style("text-align", "center")
    .style("font-size", "18px")
    .html("Error loading comet data.<br>Please ensure the file 'near-earth-comets.csv' is in the 'data' folder.<br><button id='useTestData'>Use Sample Data</button>");
  
  // Add button to use sample data as fallback
  d3.select("#useTestData").on("click", function() {
    d3.select(".error-message").remove();
    const sampleData = getSampleData();
    visualizeData(sampleData);
  });
}

// Function to create sample data as fallback
function getSampleData() {
  const sampleData = [];
  
  // Generate 15 sample comets with realistic values
  for (let i = 0; i < 15; i++) {
    sampleData.push({
      Object: `Sample Comet ${i+1}`,
      e: 0.5 + Math.random() * 0.45, // eccentricity between 0.5 and 0.95
      i: Math.random() * 180, // inclination between 0 and 180
      q: 0.5 + Math.random() * 1.5, // perihelion distance between 0.5 and 2 AU
      Q: 4 + Math.random() * 10, // aphelion distance between 4 and 14 AU
      P: 5 + Math.random() * 20, // orbital period between 5 and 25 years
      MOID: 0.01 + Math.random() * 0.5, // MOID between 0.01 and 0.51 AU
      name: `Sample Comet ${i+1}`
    });
  }
  
  // Add derived attributes
  sampleData.forEach(d => {
    d.semi_major_axis = (d.q + d.Q) / 2;
    d.semi_minor_axis = d.semi_major_axis * Math.sqrt(1 - (d.e * d.e));
  });
  
  console.log("Using sample data instead:", sampleData);
  return sampleData;
}

// Function to create category filters
function createCategoryFilters() {
  if (document.getElementById("categoryFilters")) return;
  
  const filtersContainer = document.createElement("div");
  filtersContainer.id = "categoryFilters";
  
  // Create period filters
  const periodHeading = document.createElement("span");
  periodHeading.className = "filterHeading";
  periodHeading.textContent = "Orbital Period:";
  filtersContainer.appendChild(periodHeading);
  
  const periodCategories = [
    { id: "all", label: "All Periods" },
    { id: "short", label: "Short (<20 yr)" },
    { id: "medium", label: "Medium (20-100 yr)" },
    { id: "long", label: "Long (>100 yr)" }
  ];
  
  periodCategories.forEach(category => {
    const button = document.createElement("button");
    button.className = "categoryButton" + (category.id === "all" ? " active" : "");
    button.dataset.category = category.id;
    button.dataset.type = "period";
    button.textContent = category.label;
    button.addEventListener("click", function() {
      // Update active state
      document.querySelectorAll('.categoryButton[data-type="period"]').forEach(btn => {
        btn.classList.remove("active");
      });
      this.classList.add("active");
      
      // Update filter and redraw
      selectedPeriodCategory = this.dataset.category;
      applyFilters();
    });
    filtersContainer.appendChild(button);
  });
  
  // Add space between filter groups
  filtersContainer.appendChild(document.createElement("br"));
  
  // Create inclination filters
  const inclinationHeading = document.createElement("span");
  inclinationHeading.className = "filterHeading";
  inclinationHeading.textContent = "Inclination:";
  filtersContainer.appendChild(inclinationHeading);
  
  const inclinationCategories = [
    { id: "all", label: "All Inclinations" },
    { id: "low", label: "Low (<10°)" },
    { id: "medium", label: "Medium (10-45°)" },
    { id: "high", label: "High (>45°)" }
  ];
  
  inclinationCategories.forEach(category => {
    const button = document.createElement("button");
    button.className = "categoryButton" + (category.id === "all" ? " active" : "");
    button.dataset.category = category.id;
    button.dataset.type = "inclination";
    button.textContent = category.label;
    button.addEventListener("click", function() {
      // Update active state
      document.querySelectorAll('.categoryButton[data-type="inclination"]').forEach(btn => {
        btn.classList.remove("active");
      });
      this.classList.add("active");
      
      // Update filter and redraw
      selectedInclinationCategory = this.dataset.category;
      applyFilters();
    });
    filtersContainer.appendChild(button);
  });
  
  // Insert filters before the time controls
  const controlsContainer = document.getElementById("controls");
  controlsContainer.appendChild(filtersContainer);
}

// Function to apply filters based on selected categories
function applyFilters() {
  // For each orbit and comet, determine if it should be visible
  allOrbits.forEach((orbitData, i) => {
    const { orbit, data } = orbitData;
    const comet = allComets[i].element;
    
    // Check period category
    let showByPeriod = true;
    if (selectedPeriodCategory !== "all") {
      if (selectedPeriodCategory === "short" && data.P >= 20) showByPeriod = false;
      if (selectedPeriodCategory === "medium" && (data.P < 20 || data.P > 100)) showByPeriod = false;
      if (selectedPeriodCategory === "long" && data.P <= 100) showByPeriod = false;
    }
    
    // Check inclination category
    let showByInclination = true;
    if (selectedInclinationCategory !== "all") {
      if (selectedInclinationCategory === "low" && data.i >= 10) showByInclination = false;
      if (selectedInclinationCategory === "medium" && (data.i < 10 || data.i > 45)) showByInclination = false;
      if (selectedInclinationCategory === "high" && data.i <= 45) showByInclination = false;
    }
    
    // Apply visibility
    const isVisible = showByPeriod && showByInclination;
    orbit.transition().duration(500).style("opacity", isVisible ? orbitData.originalOpacity : 0);
    comet.transition().duration(500).style("opacity", isVisible ? 0.8 : 0);
  });
}

// Separate visualization function that can be called with either real or sample data
function visualizeData(data) {
  // Reset global arrays
  allOrbits = [];
  allComets = [];
  comets = []; // Clear global comets array
  
  // Filter for data quality and visualization clarity
  data = data.filter(d => 
    d.q > 0 && 
    d.Q > 0 && 
    d.P > 0 && 
    d.P < 300 && // Limit to shorter period comets for visual clarity
    !isNaN(d.e) && 
    !isNaN(d.i)
  );

  console.log("After filtering:", data.length, "comets remain");

  // Keep a manageable number of comets for clarity
  // Take a diverse sample rather than just the first 50
  if (data.length > 50) {
    data.sort((a, b) => a.P - b.P); // Sort by orbital period
    const step = Math.floor(data.length / 50);
    const sampledData = [];
    for (let i = 0; i < data.length && sampledData.length < 50; i += step) {
      sampledData.push(data[i]);
    }
    data = sampledData;
    console.log("Sampled down to", data.length, "comets for visualization");
  }

  // Scale functions
  const orbitalPeriodScale = d3.scaleLog()
    .domain(d3.extent(data, d => d.P))
    .range([50, 300]);
  
  const moidScale = d3.scaleLinear()
    .domain(d3.extent(data, d => d.MOID))
    .range([4, 1]);
  
  const perihelionScale = d3.scaleLinear()
    .domain(d3.extent(data, d => d.q))
    .range([0.3, 1]);
  
  const colorScale = d3.scaleSequential()
    .domain(d3.extent(data, d => d.q))
    .interpolator(d3.interpolateInferno);

  // Create orbits and comet objects for each comet
  data.forEach((d, i) => {
    // Calculate orbit size based on orbital period (scaled for visualization)
    const a = orbitalPeriodScale(d.P);
    const e = d.e;
    const b = a * Math.sqrt(1 - (e * e));
    
    // Calculate orbit offset from center (Sun) based on eccentricity
    const c = a * e;
    const cx = -c; // Offset in the negative x direction
    
    // Rotation angle based on inclination (convert to radians)
    const rotationAngle = (d.i / 180) * Math.PI;
    
    // Stroke width based on MOID (closer to Earth = thicker line)
    const strokeWidth = Math.max(0.5, moidScale(d.MOID || 0.5));
    
    // Opacity based on perihelion distance (closer = more opaque)
    const opacity = perihelionScale(d.q);
    
    // Create elliptical path for orbit - Using SVG path commands instead of ellipsePath.ellipse
    // Using parametric equation for ellipse
    let pathData = "M " + (cx + a) + " 0 ";  // Start at rightmost point of the ellipse
    
    // Add elliptical arc commands for both halves of the ellipse
    pathData += "A " + a + " " + b + " 0 1 1 " + (cx - a) + " 0 ";  // First half
    pathData += "A " + a + " " + b + " 0 1 1 " + (cx + a) + " 0";   // Second half
    
    // Draw the orbit
    const orbit = g.append("path")
      .attr("class", "orbit")
      .attr("d", pathData)
      .attr("fill", "none")
      .attr("stroke", colorScale(d.q))
      .attr("stroke-width", strokeWidth)
      .attr("opacity", opacity)
      .attr("transform", `rotate(${(d.i * 2) % 180})`)
      .attr("data-name", d.name);
    
    // Store orbit data with original opacity for filtering
    allOrbits.push({
      orbit: orbit,
      data: d,
      originalOpacity: opacity
    });
    
    // Add hover interactions
    orbit.on("mouseover", function(event) {
        d3.select(this)
          .attr("stroke-width", strokeWidth * 2)
          .attr("opacity", 1);
          
        // Show tooltip with fixed positioning
        tooltip.transition()
          .duration(200)
          .style("opacity", 0.9);
        
        tooltip.html(`
          <strong>${d.name}</strong><br/>
          Period: ${d.P.toFixed(1)} years<br/>
          Perihelion: ${d.q.toFixed(2)} AU<br/>
          Eccentricity: ${d.e.toFixed(3)}<br/>
          Inclination: ${d.i.toFixed(1)}°<br/>
          MOID: ${(d.MOID || 0).toFixed(4)} AU
          <br/><small>(Click for more information)</small>
        `)
          .style("left", (event.pageX + 10) + "px")
          .style("top", (event.pageY - 28) + "px");
          
        // Play sound for this comet
        playCometSound({
          name: d.name,
          period: d.P,
          e: d.e,
          q: d.q
        });
      })
      .on("mouseout", function() {
        d3.select(this)
          .attr("stroke-width", strokeWidth)
          .attr("opacity", opacity);
        
        tooltip.transition()
          .duration(500)
          .style("opacity", 0);
      })
      .on("click", function() {
        // Show timeline for this comet
        showTimelinePanel({
          name: d.name,
          period: d.P,
          e: d.e,
          q: d.q
        });
        
        // Show mythology panel if we have data for this comet
        if (mythologyData[d.name]) {
          showMythologyPanel(d.name);
        }
      });
    
    // Create comet object for animation - RESTORE ORIGINAL IMPLEMENTATION
    const comet = g.append("circle")
      .attr("class", "comet")
      .attr("r", 4)
      .attr("fill", colorScale(d.q))
      .attr("stroke", "#FFFFFF")
      .attr("stroke-width", 1)
      .attr("opacity", 0)
      .attr("transform", `rotate(${(d.i * 2) % 180})`)
      .attr("data-name", d.name)
      .attr("filter", "url(#glow)");
    
    // Store comet data for animation and filtering
    const cometData = {
      element: comet,
      a: a,
      b: b,
      cx: cx,
      period: d.P,
      e: d.e,
      q: d.q,
      name: d.name,
      inclination: d.i
    };
    
    allComets.push(cometData);
    comets.push(cometData); // Add to global comets array for animation
  });

  // Add legend
  const legend = svg.append("g")
    .attr("class", "legend")
    .attr("transform", `translate(${width - 180}, ${height - 150})`);
  
  legend.append("rect")
    .attr("width", 160)
    .attr("height", 130)
    .attr("fill", "#050520")
    .attr("stroke", "#444")
    .attr("rx", 5)
    .attr("opacity", 0.8);
  
  legend.append("text")
    .attr("x", 80)
    .attr("y", 25)
    .attr("text-anchor", "middle")
    .attr("fill", "#FFFFFF")
    .attr("font-weight", "bold")
    .text("Legend");
  
  // Perihelion distance legend
  const gradientId = "perihelion-gradient";
  const linearGradient = legend.append("linearGradient")
    .attr("id", gradientId)
    .attr("x1", "0%")
    .attr("y1", "0%")
    .attr("x2", "100%")
    .attr("y2", "0%");
    
  const perihelionDomain = d3.extent(data, d => d.q);
  
  d3.range(0, 1.01, 0.1).forEach(d => {
    linearGradient.append("stop")
      .attr("offset", `${d * 100}%`)
      .attr("stop-color", colorScale(perihelionDomain[0] + d * (perihelionDomain[1] - perihelionDomain[0])));
  });
  
  legend.append("rect")
    .attr("x", 20)
    .attr("y", 40)
    .attr("width", 120)
    .attr("height", 10)
    .attr("fill", `url(#${gradientId})`);
    
  legend.append("text")
    .attr("x", 80)
    .attr("y", 65)
    .attr("text-anchor", "middle")
    .attr("fill", "#FFFFFF")
    .attr("font-size", "10px")
    .text("Perihelion Distance (AU)");
    
  legend.append("text")
    .attr("x", 20)
    .attr("y", 65)
    .attr("text-anchor", "middle")
    .attr("fill", "#FFFFFF")
    .attr("font-size", "9px")
    .text(perihelionDomain[0].toFixed(1));
    
  legend.append("text")
    .attr("x", 140)
    .attr("y", 65)
    .attr("text-anchor", "middle")
    .attr("fill", "#FFFFFF")
    .attr("font-size", "9px")
    .text(perihelionDomain[1].toFixed(1));
  
  // Size legend (MOID)
  legend.append("text")
    .attr("x", 80)
    .attr("y", 85)
    .attr("text-anchor", "middle")
    .attr("fill", "#FFFFFF")
    .attr("font-size", "10px")
    .text("Earth Proximity (MOID)");
    
  const moidSizes = [1, 2.5, 4];
  moidSizes.forEach((size, i) => {
    legend.append("line")
      .attr("x1", 30 + i * 50)
      .attr("y1", 100)
      .attr("x2", 50 + i * 50)
      .attr("y2", 100)
      .attr("stroke", "#FFFFFF")
      .attr("stroke-width", size);
      
    legend.append("text")
      .attr("x", 40 + i * 50)
      .attr("y", 115)
      .attr("text-anchor", "middle")
      .attr("fill", "#FFFFFF")
      .attr("font-size", "9px")
      .text(["Close", "Medium", "Far"][i]);
  });
  
  // Create category filter buttons
  createCategoryFilters();
  
  // Set up animation controls
  d3.select("#playButton").on("click", function() {
    if (animationRunning) {
      // Stop animation
      animationRunning = false;
      d3.select(this).text("Play");
    } else {
      // Start animation
      animationRunning = true;
      d3.select(this).text("Pause");
      animate();
    }
  });
  
  d3.select("#timeSlider").on("input", function() {
    currentTime = +this.value;
    updatePositions();
  });
  
  // Initialize animation
  updatePositions();
}

// Animation functions
function animate() {
  if (!animationRunning) return;
  
  currentTime += timeScale;
  if (currentTime > 100) currentTime = 0;
  
  // Update slider position
  d3.select("#timeSlider").property("value", currentTime);
  
  updatePositions();
  requestAnimationFrame(animate);
}

function updatePositions() {
  // Calculate the current year based on the time slider
  // Assuming 100 time units = 50 years (adjust as needed)
  const baseYear = 2023;
  const yearRange = 50;
  const currentYear = baseYear + (currentTime / 100) * yearRange;
  
  // Update year display
  d3.select("#yearDisplay").text(`Year: ${Math.round(currentYear)}`);
  
  // Update Earth position (rotates around the Sun)
  const earthAngle = (currentTime / 100) * 2 * Math.PI;
  earth
    .attr("cx", earthOrbitRadius * Math.cos(earthAngle))
    .attr("cy", earthOrbitRadius * Math.sin(earthAngle));
  
  // Check if comets array exists and has elements
  if (!comets || comets.length === 0) {
    console.warn("No comets to animate");
    return;
  }
  
  // Update comet positions - RESTORE ORIGINAL IMPLEMENTATION
  comets.forEach(comet => {
    // Only process if comet element exists
    if (!comet.element) return;
    
    try {
      // Calculate position along elliptical orbit
      // We use a slightly different speed for each comet based on its period
      const speed = 2 * Math.PI / (comet.period / 5); // Scale the period for visualization
      const angle = (currentTime / 100) * speed;
      
      // Parametric equation of ellipse
      const x = comet.cx + comet.a * Math.cos(angle);
      const y = comet.b * Math.sin(angle);
      
      // Distance from center (used for opacity)
      const distanceFromCenter = Math.sqrt(x * x + y * y);
      const maxDistance = Math.max(comet.a, comet.b);
      const proximityFactor = 1 - (distanceFromCenter / maxDistance);
      
      // Make comets more visible near perihelion (closest to Sun)
      const perihelionProximity = Math.cos(angle - Math.PI) * 0.5 + 0.5;
      const visibility = 0.7 * perihelionProximity + 0.3;
      
      // Apply position
      comet.element
        .attr("cx", x)
        .attr("cy", y)
        .attr("r", 3 + proximityFactor * 3)
        .attr("opacity", visibility);
        
      // Play sound when comet is close to perihelion (closest to sun)
      const angleFromPerihelion = Math.abs(angle % (2 * Math.PI) - Math.PI);
      if (angleFromPerihelion < 0.1 && soundEnabled) {
        playCometSound(comet);
      }
    } catch (error) {
      console.error("Error updating comet position:", error, comet);
    }
  });
}

// Complete the HTML content
document.addEventListener("DOMContentLoaded", function() {
  // Check if control elements exist, create them if needed
  if (!document.getElementById("controls")) {
    console.warn("Controls container not found. Creating one.");
    const controlsDiv = document.createElement("div");
    controlsDiv.id = "controls";
    controlsDiv.style.textAlign = "center";
    controlsDiv.style.margin = "20px 0";
    
    const playButton = document.createElement("button");
    playButton.id = "playButton";
    playButton.textContent = "Play";
    
    const slider = document.createElement("input");
    slider.id = "timeSlider";
    slider.type = "range";
    slider.min = 0;
    slider.max = 100;
    slider.value = 0;
    slider.style.width = "300px";
    slider.style.margin = "0 15px";
    
    const yearDisplay = document.createElement("span");
    yearDisplay.id = "yearDisplay";
    yearDisplay.textContent = "Year: 2023";
    yearDisplay.style.color = "white";
    yearDisplay.style.fontWeight = "bold";
    yearDisplay.style.marginLeft = "15px";
    yearDisplay.style.fontSize = "16px";
    
    controlsDiv.appendChild(playButton);
    controlsDiv.appendChild(slider);
    controlsDiv.appendChild(yearDisplay);
    
    // Add empty category filters container
    const filtersContainer = document.createElement("div");
    filtersContainer.id = "categoryFilters";
    controlsDiv.appendChild(filtersContainer);
    
    const visContainer = document.getElementById("vis");
    visContainer.parentNode.insertBefore(controlsDiv, visContainer);
  }
  
  // Add description if needed
  if (document.querySelector(".description") === null) {
    console.warn("Description container not found. Creating one.");
    const descDiv = document.createElement("div");
    descDiv.className = "description";
    descDiv.style.margin = "20px 0";
    descDiv.style.padding = "15px";
    descDiv.style.background = "rgba(0,0,0,0.5)";
    descDiv.style.borderRadius = "8px";
    descDiv.style.color = "white";
    
    const descP1 = document.createElement("p");
    const descP2 = document.createElement("p");
    
    descDiv.appendChild(descP1);
    descDiv.appendChild(descP2);
    
    document.getElementById("vis").parentNode.appendChild(descDiv);
  }
  
  const descriptionText = `
    This visualization represents Near Earth Comets as an "Orbital Symphony" - a cosmic ballet of orbital paths around our Sun.
    Each elliptical path represents a comet's orbit, where:
    • Size of the orbit corresponds to the comet's orbital period (P)
    • Color indicates perihelion distance (q) - closest approach to the Sun
    • Line thickness represents proximity to Earth's orbit (MOID)
    • Orbit inclination shows the tilt of the comet's orbit relative to Earth's orbital plane (i)
    • Orbit eccentricity (e) represents how elongated the comet's path is
    
    Hover over an orbit to see details about the specific comet. Click to view its timeline and mythological connections.
    Enable sound to experience the "Orbital Symphony" through audio - each comet creates a unique tone based on its properties.
    Use the slider to control time or click Play to animate the cosmic dance.
  `;
  
  document.querySelector(".description p").textContent = descriptionText;
  
  // Initialize audio system when the document is loaded
  initializeAudio();
  
  // Add sound toggle
  const soundToggleBtn = document.getElementById("soundToggle");
  if (soundToggleBtn) {
    soundToggleBtn.addEventListener("click", function() {
      soundEnabled = !soundEnabled;
      
      // Update button text
      this.textContent = soundEnabled ? "🔊 Sound On" : "🔊 Sound Off";
      this.classList.toggle("active", soundEnabled);
      
      // If enabling sound, we need to explicitly start Tone.js
      if (soundEnabled) {
        Tone.start().then(() => {
          console.log("Tone.js started successfully");
          // Play a test sound to confirm it works
          if (synth) {
            synth.triggerAttackRelease("C4", 0.3);
          }
        }).catch(err => {
          console.error("Failed to start Tone.js:", err);
        });
      }
    });
  }
  
  // Add close buttons for panels
  document.querySelectorAll(".close-button").forEach(button => {
    button.addEventListener("click", function() {
      this.parentElement.style.display = "none";
    });
  });
});