loadedInterfaceName = "monotron";

interfaceOrientation = "landscape";

pages = [[
{
    "name": "refresh",
    "type": "Button",
    "bounds": [.6, .9, .2, .1],
    "startingValue": 0,
    "isLocal": true,
    "mode": "contact",
    "ontouchstart": "interfaceManager.refreshInterface()",
    "stroke": "#aaa",
    "label": "refresh",
},
{
    "name": "tabButton",
    "type": "Button",
    "bounds": [.8, .9, .2, .1],
    "mode": "toggle",
    "stroke": "#aaa",
    "isLocal": true,
    "ontouchstart": "if(this.value == this.max) { control.showToolbar(); } else { control.hideToolbar(); }",
    "label": "menu",
},

{
    "name": "title",
    "type": "Label",
    "x": 0, "y": 0,
    "width": 1, "height": 0.1,
    "value": "monotron",
    "align": "left",
},

// Panel includes: (from left-to-right)
// standby/pitch/cutoff 3-pos-switch
{
     "name" : "standby_button",
     "type" : "Button",
     "x" : 0.04, "y" : 0.24,
     "width" : .08, "height" : .08,
    "label" : "standby",
},
{
     "name" : "pitch_button",
     "type" : "Button",
     "x" : 0.04, "y" : 0.32,
     "width" : .08, "height" : .08,
    "label" : "pitch",
},
{
     "name" : "standby_button",
     "type" : "Button",
     "x" : 0.04, "y" : 0.40,
     "width" : .08, "height" : .08,
    "label" : "cutoff",
},


// VCO pitch knob
{
    "name": "pitch_label",
    "type": "Label",
    "x": 0.167, "y": 0.2,
    "width": 0.15, "height": 0.05,
    "value": "pitch",
},
{
    "name":"pitch",
    "type":"Knob",
    "x":     0.167, "y":      0.3,
    "width": 0.15,  "height": 0.15,
},

// LFO rate knob
{
    "name": "rate_label",
    "type": "Label",
    "x": 0.333, "y": 0.2,
    "width": 0.15, "height": 0.05,
    "value": "rate",
},
{
    "name":"rate",
    "type":"Knob",
    "x":     0.333, "y":      0.3,
    "width": 0.15,  "height": 0.15,
},

// LFO int(ensity) knob
{
    "name": "int_label",
    "type": "Label",
    "x": 0.5, "y": 0.2,
    "width": 0.15, "height": 0.05,
    "value": "int",
},
{
    "name":"int",
    "type":"Knob",
    "x":     0.5, "y":      0.3,
    "width": 0.15,  "height": 0.15,
},

// VCF cutoff
{
    "name": "cutoff_label",
    "type": "Label",
    "x": 0.667, "y": 0.2,
    "width": 0.15, "height": 0.05,
    "value": "cutoff",
},
{
    "name":"cutoff",
    "type":"Knob",
    "x":     0.667, "y":      0.3,
    "width": 0.15,  "height": 0.15,
},

// VCF peak
{
    "name": "peak_label",
    "type": "Label",
    "x": 0.833, "y": 0.2,
    "width": 0.15, "height": 0.05,
    "value": "peak",
},
{
    "name":"peak",
    "type":"Knob",
    "x":     0.833, "y":      0.3,
    "width": 0.15,  "height": 0.15,
},



// "Keyboard"
{
    "name": "multi",
    "type": "MultiTouchXY",
    "x": .0,
    "y": .55,
    "width": .99,
    "height": .3,
    "color": "#555555",
    "stroke": "#aaaaaa",
    "mode": "momentary",
    "midiType": "cc",
    "midiNumber": 1,
    "maxTouches": 1,
    "isMomentary": false,
    "requiresTouchDown": false,
},



]

];
