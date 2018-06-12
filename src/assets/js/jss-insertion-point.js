import { create } from 'jss';
import { jssPreset } from 'material-ui/styles';

export default function() {
  let jss = create(jssPreset());
  if (typeof document !== "undefined") {
    jss.options.insertionPoint = document.getElementById('jss-insertion-point');
  }
  else {
    jss.options.insertionPoint = 'jss-insertion-point';
  };
  return jss;
}
