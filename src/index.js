import rough from 'roughjs';

import { Elm } from './Main.elm'

const node = document.getElementById('app')
const flags = {}
const app = Elm.Main.init({
  node,
  flags,
})


class FieldTile extends HTMLElement {
  static observedAttributes = ["size"];

  constructor() {
    super();
  }

  connectedCallback() {
    const gridSize = this.getAttribute("size")
    const height = gridSize*6
    const width = gridSize*7

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)

    const rc = rough.svg(svg);
    // x, y, width, height
    const rect = rc.rectangle(0, 0, width, height, {
      fill: "lightgreen",
      stroke: "rgb(100, 240, 100)",
      fillStyle: "cross-hatch",
    });
    svg.appendChild(rect);

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
    const height = 10
    const width = gridSize*7
    const color = this.getAttribute("color")

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
    svg.setAttribute("height", height)
    svg.setAttribute("width", width)

    const rc = rough.svg(svg);
    // x, y, width, height
    const rect = rc.rectangle(0, 0, width, height, {
      stroke: "rgb(100, 240, 100)",
      fill: color,
      fillStyle: 'solid'
    });
    svg.appendChild(rect);

    this.appendChild(svg)
  }

}
if (customElements.get( 'field-line' ) == null) {
  customElements.define("field-line", FieldLine);
}
