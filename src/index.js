import rough from 'roughjs';
import {Howl, Howler} from 'howler';


import { Elm } from './Main.elm'

const node = document.getElementById('app')
const flags = {
  seed: Date.now(),
}
const app = Elm.Main.init({
  node,
  flags,
})

const sounds = {
  theme: new Howl({
    src: [require("url:./assets/theme.mp3")],
    loop: true,
  }),
  whistle: new Howl({
    src: [require("url:./assets/whistle.mp3")],
  })
}


app.ports.playHowlerSound.subscribe(msg => {
  console.log(msg.id)
  switch (msg.id) {
    case "theme":
      if (msg.play) {
        sounds.theme.play()
      } else {
        sounds.theme.stop()
      }
      break
    case "whistle":
      sounds.whistle.play()
      break
    default:
  }
})

const windowHeight = window.innerHeight
const windowWidth = window.innerWidth

const svgWrapper = document.createElement('div')
svgWrapper.style.position = "absolute"
svgWrapper.style.width = "absolute"
svgWrapper.style.top = 0
svgWrapper.style.zIndex = -2
svgWrapper.style.height = "100%"


const domSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
domSvg.setAttribute("height", windowHeight)
domSvg.setAttribute("width", windowWidth)
svgWrapper.appendChild(domSvg)
document.body.appendChild(svgWrapper)

const rc = rough.svg(domSvg);
const rect = rc.rectangle(
  8,
  8,
  windowWidth - 16,
  windowHeight - 16,
  {
    fill: "green",
    roughness: 2,
  })

domSvg.appendChild(rect)


class FieldTriangle extends HTMLElement {
  static observedAttributes = ["direction"];

  constructor() {
    super();
  }

  connectedCallback() {
    const direction = this.getAttribute("direction")

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", "12px")
    svg.setAttribute("width", "18px")

    const vertices =
        direction === "right"
          ? [[0,0],[0,12],[18, 6], [0, 0]]
          : [[18,0],[18,12],[0, 6], [18, 0]]


    const rc = rough.svg(svg);
    const tri = rc.polygon(vertices, {
      stroke: "black",
      fill: "white",
      fillStyle: "solid",
    });
    svg.appendChild(tri);

    this.appendChild(svg)
  }
}
if (customElements.get('field-triangle') == null) {
  customElements.define("field-triangle", FieldTriangle);
}

class RoughModal extends HTMLElement {
  static observedAttributes = ["height"];
  static observedAttributes = ["width"];
  static observedAttributes = ["transparent"];

  constructor() {
    super();
  }

  connectedCallback() {
    const height = this.getAttribute("height")
    const width = this.getAttribute("width")
    const transparent = this.getAttribute("transparent") === "true"

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)
    svg.style.backgroundColor = transparent ? "transparent" : "white"

    const rc = rough.svg(svg);
    // x, y, width, height
    const rect = rc.rectangle(2, 2, width - 4, height - 4, {
      stroke: "black",
      fill: "rgba(255, 255, 255, 0.8)",
      fillStyle: "solid"
    });
    svg.appendChild(rect);

    this.appendChild(svg)
  }

}
if (customElements.get( 'rough-modal' ) == null) {
  customElements.define("rough-modal", RoughModal);
}




class FieldTile extends HTMLElement {
  static observedAttributes = ["size"];

  constructor() {
    super();
  }

  connectedCallback() {
    const gridSize = this.getAttribute("size")
    const height = (gridSize*6)+4
    const width = (gridSize*7)+4

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)
    svg.style.backgroundColor = "white"

    const rc = rough.svg(svg);
    // x, y, width, height
    const rect = rc.rectangle(2, 2, width - 4, height - 4, {
      fill: "lightgreen",
      stroke: "green",
      fillStyle: "cross-hatch",
    });
    svg.appendChild(rect);

    // Add row lines
    for (let i=1; i <= 5; i++) {
      const line = rc.line(2, i*gridSize, width-4, i*gridSize, { stroke: 'rgba(0,0,0,0.1)' })
      svg.appendChild(line)
    }

    // Add column lines
    for (let i=1; i <= 6; i++) {
      const line = rc.line(i*gridSize, 2, i*gridSize, height-4, { stroke: 'rgba(0,0,0,0.1)' })
      svg.appendChild(line)
    }


    this.appendChild(svg)
  }

}
if (customElements.get( 'field-tile' ) == null) {
  customElements.define("field-tile", FieldTile);

}

class FieldLine extends HTMLElement {
  static observedAttributes = ["size", "color"];

  constructor() {
    super();
  }

  connectedCallback() {
    const gridSize = this.getAttribute("size")
    const height = 12
    const width = (gridSize*7)+4
    const color = this.getAttribute("color")

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)

    const rc = rough.svg(svg);
    // x, y, width, height
    const rect = rc.rectangle(2, 0, width-4, height, {
      stroke: "green",
      fill: color,
      fillStyle: 'solid'
    });
    svg.appendChild(rect);

    this.appendChild(svg)
    this.style.transform = "translateY(-6px)"
  }

}
if (customElements.get( 'field-line' ) == null) {
  customElements.define("field-line", FieldLine);
}

class FieldNumber extends HTMLElement {
  static observedAttributes = ["size", "number"];

  constructor() {
    super();
  }

  connectedCallback() {
    const gridSize = this.getAttribute("size")
    const scaleFactor = ((gridSize/80))
    const height = 80*scaleFactor + 4
    const width = 50*scaleFactor + 4
    const yardTen = this.getAttribute("number")

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)

    const rc = rough.svg(svg);
    const paths = this.pathsForNumber(yardTen)
    for (let i=0; i < paths.length; i++ ) {
      const section = rc.polygon(this.scaleText(gridSize, paths[i]), { fill: "white", fillStyle: 'solid' })
      svg.appendChild(section);
    }

    this.appendChild(svg)
  }

  scaleText(gridSize, path) {
    return path.map(([x,y]) => {
      return [
        ((x-2)*(gridSize*0.8/80)) + 2,
        ((y-2)*(gridSize*0.8/80)) + 2,
      ]
    })

  }

  /* All these were created
   * based on text being 80x50
   * */
  pathsForNumber(number) {
    switch(number) {
      case "0":
        return [
            [[10.728,2],[2,10.728],[2.025,73.297],[10.728,82],[44.316,82],[53.126,73.19],[53.126,10.81],[44.316,2],[10.728,2]],
            [[34.038,64.979],[21.117,64.979],[21.117,19.021],[34.038,19.021],[34.038,64.979]],
        ]
      case "1":
        return [
          [[25.878,2],[2,2],[2,19.022],[6.762,19.022],[6.762,64.979],[2,64.979],[2,82],[30.654,82],[30.654,64.979],[25.891,64.979],[25.878,2]]
        ]
      case "2":
        return [
          [[21.089,27.626],[21.089,21.408],[24.025,19.022],[31.073,19.022],[34.04,21.408],[34.04,31.096],[31.032,33.49],[10.729,33.49],[2,42.2],[2,82],[53.126,82],[53.126,56.376],[34.04,56.376],[34.04,64.98],[21.089,64.98],[21.089,52.904],[24.096,50.51],[44.399,50.51],[53.126,41.802],[53.126,10.728],[44.399,2],[10.81,2],[2,10.81],[2,27.626],[21.089,27.626]]
        ]
      case "3":
        return [
          [[53.126,10.728],[44.399,2],[10.81,2],[2,10.81],[2,27.626],[21.089,27.626],[21.089,21.408],[24.025,19.021],[31.073,19.021],[34.04,21.408],[34.04,31.097],[31.032,33.49],[21.089,33.49],[21.089,50.51],[31.032,50.51],[34.04,52.904],[34.04,62.592],[31.073,64.98],[24.025,64.98],[21.089,62.592],[21.089,56.376],[2,56.376],[2,73.19],[10.81,82],[44.399,82],[53.126,73.272],[53.126,46.354],[48.763,42.001],[53.126,37.647],[53.126,10.728]]
        ]
      case "4":
        return [
          [[53.126,42.001],[48.362,42.001],[48.361,2],[16.906,2],[2,42.001],[2,59.021],[29.236,59.021],[29.236,64.979],[24.473,64.979],[24.473,82],[53.126,82],[53.126,64.979],[48.363,64.979],[48.363,59.021],[53.126,59.021],[53.126,42.001]],
          [[22.718,42.001],[29.236,24.054],[29.236,42.001],[22.718,42.001]],
       ]
      case "5":
        return [
          [[2,56.376],[2,73.19],[10.81,82],[44.399,82],[53.126,73.272],[53.126,42.2],[44.399,33.49],[21.089,33.49],[21.089,19.021],[34.04,19.021],[34.04,27.626],[53.126,27.626],[53.126,2],[2,2],[2,50.51],[31.032,50.51],[34.04,52.904],[34.04,62.592],[31.074,64.98],[24.025,64.98],[21.089,62.592],[21.089,56.376],[2,56.376]]
        ]
      case "6":
        return [
          [[53.126,10.81],[44.316,2],[10.728,2],[2,10.728],[2,73.272],[10.728,82],[44.316,82],[53.126,73.19],[53.126,42.297],[44.316,33.486],[21.089,33.486],[21.089,21.408],[24.054,19.021],[31.102,19.021],[34.038,21.408],[34.038,27.625],[53.126,27.625],[53.126,10.81]],
          [[31.103,50.507],[34.04,52.894],[34.04,62.591],[31.103,64.977],[24.055,64.977],[21.09,62.591],[21.09,50.507],[31.103,50.507]],
        ]
      case "7":
        return [
          [[53.127,2],[2,2],[2,27.626],[21.089,27.626],[21.089,19.022],[34.012,19.022],[16.307,64.98],[7.947,64.98],[7.947,82],[41.372,82],[41.372,64.98],[35.436,64.98],[53.127,19.022],[53.127,2]]
        ]
      case "8":
        return [
          [[44.399,2],[10.81,2],[2,10.728],[2,37.647],[6.363,42.001],[2,46.353],[2,73.19],[10.81,82],[44.399,82],[53.126,73.19],[53.126,46.354],[48.763,42.002],[53.126,37.648],[53.126,10.728],[44.399,2]],
          [[34.04,62.592],[31.074,64.979],[24.055,64.979],[21.09,62.592],[21.09,52.904],[24.097,50.51],[31.033,50.51],[34.041,52.904],[34.041,62.592],[34.04,62.592]],
          [[34.04,31.097],[31.032,33.491],[24.096,33.491],[21.089,31.097],[21.089,21.41],[24.054,19.023],[31.073,19.023],[34.038,21.41],[34.038,31.097],[34.04,31.097]],
        ]
      case "9":
        return [
          [[2.001,56.375],[2.001,73.19],[10.811,82],[44.399,82],[53.127,73.273],[53.127,10.727],[44.398,2],[10.81,2],[2,10.81],[2,41.704],[10.81,50.514],[34.037,50.514],[34.037,62.591],[31.073,64.979],[24.024,64.979],[21.088,62.591],[21.088,56.375],[2.001,56.375]],
          [[24.024,33.492],[21.088,31.106],[21.088,21.409],[24.024,19.022],[31.073,19.022],[34.037,21.409],[34.037,33.492],[24.024,33.492]],
        ]
      default:
        return []
    }

  }

}
if (customElements.get( 'field-number' ) == null) {
  customElements.define("field-number", FieldNumber);
}

class Target extends HTMLElement {
  static observedAttributes = ["gridSize"];

  constructor() {
    super();
  }

  connectedCallback() {
    const size = this.getAttribute("size")

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", size)
    svg.setAttribute("width", size)

    const rc = rough.svg(svg);
    // Note: For this path, it's specifically based on a 72 size grid
    //                so the path will have to be recreated if the grid size changes
    //                I made it by creating two circles and unioning them, which roughjs can't do
    const outer = rc.path("M 57.429 36 C 57.429 47.835 47.835 57.429 36 57.429 C 24.166 57.429 14.571 47.835 14.571 36 C 14.571 24.166 24.166 14.571 36 14.571 C 47.835 14.571 57.429 24.166 57.429 36 Z M 36 0 C 16.118 0 0 16.118 0 36 C 0 55.882 16.118 72 36 72 C 55.882 72 72 55.882 72 36 C 72 16.118 55.882 0 36 0 Z", {
      stroke: "red",
      fill: "red",
      fillStyle: "cross-hatched",
    });
    svg.appendChild(outer);
    const inner = rc.circle(size/2,size/2,size/6, {
      stroke: "red",
      fill: "red",
      fillStyle: "cross-hatched",
    });
    svg.appendChild(inner);
     this.appendChild(svg)
  }
}
if (customElements.get('field-target') == null) {
  customElements.define("field-target", Target);
}
