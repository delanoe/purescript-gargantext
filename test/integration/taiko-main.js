const { openBrowser, goto, write, click, closeBrowser, $, into } = require('taiko');

(async () => {
    try {
        await openBrowser();
        await goto("http://localhost:8008");
        await click($('#page-wrapper .btn-primary'));
	
	await write('user1', into($('#loginModal input[name=username]')));
	await write('1resu', into($('#loginModal input[name=password]')));
	await click($('#loginModal input[type=checkbox]'));
        await click($('#loginModal #login-button'));
    } catch (error) {
            console.error(error);
    } finally {
            closeBrowser();
    }
})();
