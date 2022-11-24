'use strict';
/**
 * @name yearToDate
 * @param {string} year
 * @returns {Date}
 */
export function yearToDate(year) {
  var d = new Date();

  d.setYear(parseInt(year));
  d.setMonth(0);
  d.setDate(1);

  return d;
}
/**
 * @name stringToDate
 * @param {string} str
 * @returns {Date}
 */
export function stringToDate(str) {
  var arr = (str.replace('"','')).split('-');
  var d = new Date();

  d.setYear(parseInt(arr[0]));
  d.setMonth(parseInt(arr[1]));
  d.setMonth(d.getMonth() - 1);
  d.setDate(parseInt(arr[2]));

  return d;
}
/**
 * @name utcStringToDate
 * @param {string} str
 * @returns {Date}
 */
export function utcStringToDate(str) {
  var arr = ((str.replace('"','')).replace(' UTC','')).split(/[\s-:]+/);
  var d = new Date();

  d.setYear(parseInt(arr[0]));
  d.setMonth(parseInt(arr[1]));
  d.setDate(parseInt(arr[2]));
  d.setHours(parseInt(arr[3]), parseInt(arr[4]), parseInt(arr[5]))

  return d;
}
