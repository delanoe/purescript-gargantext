'use strict';

/**
 * @function add
 * @param {Window} window
 * @param {Document} document
 * @param {String} sourceQuery
 * @param {String} targetQuery
 * @param {String} type
 */
export function _add(window, document, sourceQuery, targetQuery, type) {
  var source = document.querySelector(sourceQuery);
  var target = document.querySelector(targetQuery);

  source.addEventListener('mousedown', initialiseResize, false);

  function initialiseResize(e) {
    window.addEventListener('mousemove', startResizing, false);
    window.addEventListener('mouseup', stopResizing, false);
  }

  function startResizing(e) {
    var height = e.clientY - target.offsetTop
    var width = e.clientX - target.offsetLeft

    if (type === 'both' || type === 'horizontal')
      target.style.height = height + 'px';
    if (type === 'both' || type === 'vertical')
      target.style.width = width + 'px';

    // prevent "user-select" highlights
    document.body.classList.add('no-user-select');
    // prevent event focus losing (eg. while hovering iframe, see #422)
    document.body.classList.add('no-pointer-events');
  }

  function stopResizing(e) {
    window.removeEventListener('mousemove', startResizing, false);
    window.removeEventListener('mouseup', stopResizing, false);

    document.body.classList.remove('no-user-select');
    document.body.classList.remove('no-pointer-events');
  }
}
/**
 * @function remove
 * @param {Document} document
 * @param {String} sourceQuery
 */
export function _remove(document, sourceQuery) {
  var source = document.querySelector(sourceQuery);
  console.log(sourceQuery)
  source.removeEventListener('mousedown');
}
