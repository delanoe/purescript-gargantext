'use strict';

/**
 * @function show
 * @param {Window} window
 * @param {string} querySelector
 * @unpure {Object} window.$
 */
export function _show(window, querySelector) {
  window.$(querySelector).modal('show');
}
/**
 * @function hide
 * @param {Window} window
 * @param {string} querySelector
 * @unpure {Object} window.$
 */
export function _hide(window, querySelector) {
  window.$(querySelector).modal('hide');
  // @XXX Bootstrap not removing some modal elements on "hide" method
  // @https://stackoverflow.com/questions/50168312/bootstrap-4-close-modal-backdrop-doesnt-disappear
  window.$('body').removeClass('modal-open');
  window.$('body').css('padding-right', '0');
  window.$('.modal-backdrop').remove();
}
