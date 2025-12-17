const { test, expect } = require('@playwright/test');

test.describe('Mega Game', () => {
  test('should have random enemy placement', async ({ page }) => {
    // First run
    await page.goto('file:///app/src/Core/index.html');
    const skipButton1 = page.locator('#gamertagSkip');
    if (await skipButton1.isVisible()) {
      await skipButton1.click();
    }
    await page.locator('.gameCard[data-game="megagame"]').click();
    await page.locator('#megaLives3').click();
    await page.waitForTimeout(1000); // Wait for game to load
    await page.screenshot({ path: 'enemy_variety_1.png' });

    // Second run
    await page.reload();
    const skipButton2 = page.locator('#gamertagSkip');
    if (await skipButton2.isVisible()) {
      await skipButton2.click();
    }
    await page.locator('.gameCard[data-game="megagame"]').click();
    await page.locator('#megaLives3').click();
    await page.waitForTimeout(1000); // Wait for game to load
    await page.screenshot({ path: 'enemy_variety_2.png' });
  });
});