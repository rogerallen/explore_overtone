// monotron interface for http://charlie-roberts.com/Control/
loadedInterfaceName = "monotron";

interfaceOrientation = "landscape";

// 3-position switch state & logic.
// state = 0/1/2 for standbay/pitch/cutoff
var toggle3pos_state = 0;
var toggle3pos_dirty = true;
control.toggle3pos = function(next_state) {
    //alert('here is: ' + next_state);
    if(next_state === toggle3pos_state) {
        // you are pushing the same button again.
        // keep this on, rather than letting it toggle
        if(next_state === 0) {
            standby_button.setValue(127);
        } else if(next_state === 1) {
            pitch_button.setValue(127);
        } else { // cutoff
            cutoff_button.setValue(127);
        }
        // only send if dirty
        if(toggle3pos_dirty) {
            midiManager.sendMIDI("cc", 1, 10, toggle3pos_state);
            toggle3pos_dirty = false;
        }
    } else {
        toggle3pos_state = next_state;
        if(next_state === 0) {
            standby_button.setValue(127);
            pitch_button.setValue(0);
            cutoff_button.setValue(0);
        } else if(next_state === 1) {
            standby_button.setValue(0);
            pitch_button.setValue(127);
            cutoff_button.setValue(0);
        } else { // cutoff
            standby_button.setValue(0);
            pitch_button.setValue(0);
            cutoff_button.setValue(127);
        }
        midiManager.sendMIDI("cc", 1, 10, toggle3pos_state);
    }
}

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

// Title Lable
{
    "name": "title",
    "type": "Label",
    "x": 0, "y": 0,
    "width": 1, "height": 0.1,
    "value": "monotron-alike",
    "align": "left",
},

// Volume knob = CC 7 
{
    "name": "volume_label",
    "type": "Label",
    "x": 0.5, "y": 0.0,
    "width": 0.1, "height": 0.05,
    "value": "volume",
},
{
    "name":"volume",
    "type":"Knob",
    "x":     0.5, "y":      0.0,
    "width": 0.1,  "height": 0.1,
    "midiNumber" : 7,
},

// Panel includes: (from left-to-right)
// standby/pitch/cutoff 3-pos-switch
// sends "junk" on 9, but real CC will go on 10
// CC 10 0/1/2 for standbay/pitch/cutoff
{
    "name" : "standby_button",
    "type" : "Button",
    "x" : 0.04, "y" : 0.24,
    "width" : .08, "height" : .08,
    "label" : "standby",
    "midiNumber" : 9,
    "midiStartingValue" : 127,
    "ontouchstart": "control.toggle3pos(0,this.value)"
},
{
    "name" : "pitch_button",
    "type" : "Button",
    "x" : 0.04, "y" : 0.32,
    "width" : .08, "height" : .08,
    "label" : "pitch",
    "midiNumber" : 9,
    "ontouchstart": "control.toggle3pos(1,this.value)"
},
{
    "name" : "cutoff_button",
    "type" : "Button",
    "x" : 0.04, "y" : 0.40,
    "width" : .08, "height" : .08,
    "label" : "cutoff",
    "midiNumber" : 9,
    "ontouchstart": "control.toggle3pos(2,this.value)"
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
    "midiNumber" : 0,
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
    "midiNumber" : 4,
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
    "midiNumber" : 3,
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
    "midiNumber" : 1,
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
    "midiNumber" : 2,
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
    "midiNumber": 5,
    "maxTouches": 1,
    "isMomentary": false,
    "requiresTouchDown": false,
},



]

];
