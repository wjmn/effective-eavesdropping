// IMPORTANT
// This file is not used in the final build process. It is only used for the live development server.
// Don't delete this file and don't bother editing it (it won't modify your final build). 

import { Elm } from './Main.elm';
import './main.css';

Elm.Main.init({
  node: document.getElementById('root')
});