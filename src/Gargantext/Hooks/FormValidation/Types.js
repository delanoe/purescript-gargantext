'use strict';
/**
 *  Email Pattern
 *        - provide a custom pattern based on regexp for email validation [1]
 *        - regexp based on RFC 2822 simplified version (see FCT-68) [2]
 *  @link https://validatejs.org/#validators-email [1]
 *  @link https://gist.github.com/gregseth/5582254 [2]
 *  @type {RegExp}
 */
 export const emailPattern = /[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/;
/**
 *  Date Pattern
 *  @link https://www.regextester.com/96683
 *  @type {RegExp}
 */
export const datePattern = /([12]\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01]))/;
