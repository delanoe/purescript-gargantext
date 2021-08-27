/* globals gauge*/
"use strict";
const path = require('path');
const {
    openBrowser,
    write,
    closeBrowser,
    goto,
    press,
    screenshot,
    above,
    click,
    checkBox,
    listItem,
    toLeftOf,
    link,
    text,
    into,
    textBox,
    evaluate,
    $
} = require('taiko');
const assert = require("assert");
const headless = process.env.headless_chrome.toLowerCase() === 'true';

beforeSuite(async () => {
    await openBrowser({
        headless: headless
    })
});

afterSuite(async () => {
    await closeBrowser();
});

// Return a screenshot file name
gauge.customScreenshotWriter = async function () {
    const screenshotFilePath = path.join(process.env['gauge_screenshots_dir'],
        `screenshot-${process.hrtime.bigint()}.png`);

    await screenshot({
        path: screenshotFilePath
    });
    return path.basename(screenshotFilePath);
};

step('Open gargantext', async () => {
    goto('http://localhost:8008');
});

step("User must be logged in as <user> with password <password>", async (user, password) => {
    await click($('.forest-layout button.btn-primary'));
    await click($('.modal-body table tr:first-child td:nth-child(2) a'));
    
    await write(user, into($('#loginModal input[name=username]')));
    await write(password, into($('#loginModal input[name=password]')));
    await click($('#loginModal input[type=checkbox]'));

    await click($('#loginModal #login-button'));
});

step("Expand tree", async () => {
    var exists;
    var $element = '.forest-layout .leaf .chevron-icon .fa-chevron-right';
    while(1) {
	exists = await $($element).exists();
	if(exists) {
	    await click($($element));
	} else {
	    break;
	}
    }
});

step('Open the list view', async () => {
    await click($('.forest-layout .fa-list'));
});
