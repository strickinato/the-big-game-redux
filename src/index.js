import { Elm } from './Main.elm'

const node = document.getElementById('app')
const flags = {}
const app = Elm.Main.init({
  node,
  flags,
})

