const {Builder, By, Key, until} = require('selenium-webdriver');

(async function login() {
    let driver = await new Builder().forBrowser('firefox').build();
    try {
	await driver.get('http://localhost:8008');

	await driver.findElement(By.css('#page-wrapper .btn-primary')).click();

	await driver.findElement(By.css('#loginModal input[name=username]')).sendKeys('user1');
	await driver.findElement(By.css('#loginModal input[name=password]')).sendKeys('1resu');
	await driver.findElement(By.css('#loginModal input[type=checkbox]')).click();
	await driver.findElement(By.css('#loginModal #login-button')).click();
    }
    finally {
	driver.quit();
    }
})();
