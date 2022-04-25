exports._add = add;
exports._remove = remove;
/**
 * @name add
 * @param {Window} window
 * @param {Document} document
 * @param {String} sourceQuery
 * @param {String} targetQuery
 * @param {String} type
 */
function add(window, document, sourceQuery, targetQuery, type) {
  var source = document.querySelector(sourceQuery);
  var target = document.querySelector(targetQuery);

  source.addEventListener('mousedown', initialiseResize, false);

  function initialiseResize(e) {
    window.addEventListener('mousemove', startResizing, false);
    window.addEventListener('mouseup', stopResizing, false);
  }

  function startResizing(e) {
    if (type === 'both' || type === 'horizontal')
      target.style.height = (e.clientY - target.offsetTop) + 'px';
    if (type === 'both' || type === 'vertical')
      target.style.width = (e.clientX - target.offsetLeft) + 'px';

    // prevent "user-select" highlights
    document.body.classList.add('no-user-select');
  }

  function stopResizing(e) {
    window.removeEventListener('mousemove', startResizing, false);
    window.removeEventListener('mouseup', stopResizing, false);

    document.body.classList.remove('no-user-select');
  }
}
/**
 * @name remove
 * @param {Document} document
 * @param {String} sourceQuery
 */
function remove(document, sourceQuery) {
  var source = document.querySelector(sourceQuery);
  console.log(sourceQuery)
  source.removeEventListener('mousedown');
}
