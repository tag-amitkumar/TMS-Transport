/* ------------------------------------------------------------------
 * Capture the handbook screenshots.
 *
 *   1. start the app:   Rscript -e "shiny::runApp('.', port=8788)"
 *   2. cd tools && npm install
 *   3. node capture-screenshots.js
 *
 * Writes docs/img/*.png — one per screen, plus a few dialogs and
 * role-scoped views. Re-run after a UI change to refresh the handbook.
 *
 * Uses puppeteer-core against the Chrome already installed on the machine
 * rather than puppeteer, which would download a second ~150 MB Chromium.
 * ------------------------------------------------------------------ */

const fs = require('fs');
const path = require('path');
const puppeteer = require('puppeteer-core');

const URL = process.env.TMS_URL || 'http://127.0.0.1:8788';
const OUT = path.join(__dirname, '..', 'docs', 'img');

// Common install locations; override with CHROME_PATH.
const CHROME_CANDIDATES = [
  process.env.CHROME_PATH,
  'C:/Program Files/Google/Chrome/Application/chrome.exe',
  'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe',
  'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe',
  '/usr/bin/google-chrome',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
].filter(Boolean);

const chrome = CHROME_CANDIDATES.find(p => { try { return fs.existsSync(p); } catch { return false; } });
if (!chrome) { console.error('No Chrome found. Set CHROME_PATH.'); process.exit(1); }

const sleep = ms => new Promise(r => setTimeout(r, ms));

/* Wait until Shiny has stopped recalculating.
 *
 * A fixed sleep is not enough: the heavier screens (Live GPS, Reports) finish
 * well after the DOM first appears, and a screenshot taken mid-render catches
 * grey placeholder boxes. This polls Shiny's own busy signals instead, then
 * adds a short settle for leaflet tiles and plotly transitions. */
/* Wait for the page to stop changing.
 *
 * Three separate signals, because no one of them is sufficient:
 *
 *  - `shiny-busy` clears while renderUI output is still arriving.
 *  - Waiting for DataTables to draw is useless if none exist *yet* — the naive
 *    "every table has rows" test passes trivially against an empty page, which
 *    is how Payments got photographed with nothing but its footnote.
 *  - So the real signal is stability: sample the rendered text length until it
 *    stops moving, then confirm any tables that did appear have drawn.
 */
async function waitIdle(page, settle = 800) {
  await page.waitForFunction(() => {
    const busy = document.documentElement.classList.contains('shiny-busy') ||
                 document.body.classList.contains('shiny-busy');
    return !busy && document.querySelectorAll('.recalculating').length === 0;
  }, { timeout: 30000 }).catch(() => {});

  await page.waitForFunction(() => {
    const el = document.querySelector('.tms-page') || document.body;
    const len = el.innerText.length;
    const n = window.__stableCount || 0;
    if (window.__lastLen === len) {
      window.__stableCount = n + 1;
    } else {
      window.__lastLen = len;
      window.__stableCount = 0;
    }
    return window.__stableCount >= 3 && len > 80;
  }, { timeout: 25000, polling: 350 }).catch(() => {});

  await page.evaluate(() => { window.__stableCount = 0; window.__lastLen = -1; });

  // Now that content has settled, any table present must have drawn its rows.
  await page.waitForFunction(() => {
    const tables = Array.from(document.querySelectorAll('.tms-table table.dataTable'))
      .filter(t => t.offsetParent !== null);
    return tables.every(t => t.querySelectorAll('tbody tr td').length > 0);
  }, { timeout: 15000 }).catch(() => {});

  await sleep(settle);
}

async function shot(page, name, opts = {}) {
  const file = path.join(OUT, name + '.png');
  await page.screenshot({ path: file, fullPage: !!opts.fullPage });
  const kb = Math.round(fs.statSync(file).size / 1024);
  console.log(`  ${name}.png  ${kb} KB`);
}

async function go(page, id) {
  await page.evaluate(p => Shiny.setInputValue('nav_go', p, { priority: 'event' }), id);
  await waitIdle(page);
}

// Click the first row of the main list so the 360° panel has a subject —
// an empty "Select a row" panel makes a poor illustration.
async function selectFirstRow(page) {
  const sel = '.tms-table table.dataTable tbody tr';
  await page.waitForSelector(sel + ' td', { timeout: 20000 }).catch(() => {});

  /* Must be a real mouse click, not element.click(). DataTables binds its
   * row-selection handler through jQuery delegation and Shiny only learns of a
   * selection from that handler — a synthetic .click() leaves the 360° panel
   * showing its "Select a row" placeholder. */
  const box = await page.evaluate(s => {
    const tr = document.querySelector(s);
    if (!tr) return null;
    tr.scrollIntoView({ block: 'center' });
    const r = tr.getBoundingClientRect();
    return { x: r.x + Math.min(120, r.width / 2), y: r.y + r.height / 2 };
  }, sel);

  if (!box) { console.warn('    ! no row to select'); return false; }
  await page.mouse.click(box.x, box.y);
  await waitIdle(page);

  const selected = await page.evaluate(s => !!document.querySelector(s + '.selected'), sel);
  if (!selected) console.warn('    ! row did not select');
  return selected;
}

async function signInAs(page, role) {
  await page.goto(URL, { waitUntil: 'networkidle2' });
  await page.waitForSelector('.login-role-chip', { timeout: 30000 });
  await waitIdle(page, 400);
  await page.evaluate(r => {
    document.querySelectorAll('.login-role-chip').forEach(c => {
      if (c.textContent.trim() === r) c.click();
    });
  }, role);
  await page.waitForSelector('.tms-shell', { timeout: 30000 });
  await waitIdle(page, 1400);
}

// Screens whose detail panel is worth showing populated.
const WITH_DETAIL = new Set([
  'branches', 'clients', 'vendors', 'consignments', 'trips',
  'vehicles', 'drivers', 'invoices', 'hrms_employees', 'bookings', 'users',
]);

const PAGES = [
  'dashboard', 'branches', 'users', 'clients', 'vendors',
  'bookings', 'booking_new', 'cargo_moto', 'consignments', 'consign_multi',
  'trips', 'ewaybill', 'pod', 'tracking', 'livegps',
  'vehicles', 'drivers', 'pincodes', 'complaints', 'invoices', 'payments',
  'hrms_employees', 'hrms_attendance', 'hrms_payroll', 'reports',
  'portal_branch', 'portal_customer', 'portal_vendor', 'security', 'settings',
];

(async () => {
  fs.mkdirSync(OUT, { recursive: true });

  const browser = await puppeteer.launch({
    executablePath: chrome,
    headless: 'new',
    defaultViewport: { width: 1600, height: 1000, deviceScaleFactor: 1 },
    args: ['--hide-scrollbars', '--force-device-scale-factor=1'],
  });
  const page = await browser.newPage();
  page.setDefaultTimeout(45000);

  console.log('Login');
  await page.goto(URL, { waitUntil: 'networkidle2' });
  await page.waitForSelector('.login-role-chip', { timeout: 30000 });
  await waitIdle(page, 600);
  await shot(page, '01-login');

  console.log('Signing in as Super Admin');
  await signInAs(page, 'Super Admin');

  /* Grab a real consignment number off the register, so the tracking screen
   * can be photographed with an actual result rather than an empty form. */
  await go(page, 'consignments');
  const sampleCN = await page.evaluate(() => {
    const cell = document.querySelector('.tms-table table.dataTable tbody tr td .cell-2 .l1');
    return cell ? cell.textContent.trim() : null;
  });
  console.log('  sample consignment for tracking:', sampleCN || '(none found)');

  console.log('Screens');
  for (const id of PAGES) {
    await go(page, id);

    // Tracking opens on an empty form — run a real lookup so the delivery
    // timeline, the whole point of the screen, is actually visible.
    if (id === 'tracking' && sampleCN) {
      await page.evaluate(cn => {
        const input = document.querySelector('input[id$="q_cn"]');
        if (input) {
          input.value = cn;
          input.dispatchEvent(new Event('input', { bubbles: true }));
          input.dispatchEvent(new Event('change', { bubbles: true }));
        }
      }, sampleCN);
      await sleep(600);
      await page.evaluate(() => {
        const b = Array.from(document.querySelectorAll('button, .btn'))
          .find(el => /Track Shipment/i.test(el.textContent));
        if (b) b.click();
      });
      await waitIdle(page, 1400);
    }

    if (WITH_DETAIL.has(id)) await selectFirstRow(page);
    await shot(page, 'screen-' + id);
  }

  /* ---- dialogs worth documenting ---- */

  console.log('Dialogs');

  // Allocation dialog: the single most important action in the app.
  await go(page, 'cargo_moto');
  await page.evaluate(() => {
    const b = Array.from(document.querySelectorAll('button, .btn'))
      .find(el => /Allocate Cargo/i.test(el.textContent));
    if (b) b.click();
  });
  await sleep(1200); await waitIdle(page, 1200);
  await shot(page, 'dialog-allocate');
  await page.keyboard.press('Escape'); await sleep(700);

  // Register-complaint dialog.
  await go(page, 'complaints');
  await page.evaluate(() => {
    const b = Array.from(document.querySelectorAll('button, .btn'))
      .find(el => /Register Complaint/i.test(el.textContent));
    if (b) b.click();
  });
  await sleep(1000); await waitIdle(page, 900);
  await shot(page, 'dialog-complaint');
  await page.keyboard.press('Escape'); await sleep(700);

  // Record-receipt dialog, showing the reconciliation hint.
  await go(page, 'payments');
  await page.evaluate(() => {
    const b = Array.from(document.querySelectorAll('button, .btn'))
      .find(el => /Record Receipt/i.test(el.textContent));
    if (b) b.click();
  });
  await sleep(1200); await waitIdle(page, 1000);
  await shot(page, 'dialog-receipt');
  await page.keyboard.press('Escape'); await sleep(700);

  /* ---- role-scoped views ---- */

  console.log('Role views');

  await signInAs(page, 'Accountant');
  await shot(page, 'role-accountant');

  await signInAs(page, 'Customer');
  await shot(page, 'role-customer');

  await signInAs(page, 'Vendor');
  await shot(page, 'role-vendor');

  await browser.close();

  const files = fs.readdirSync(OUT).filter(f => f.endsWith('.png'));
  const total = files.reduce((s, f) => s + fs.statSync(path.join(OUT, f)).size, 0);
  console.log(`\n${files.length} images, ${(total / 1024 / 1024).toFixed(1)} MB total`);
})().catch(e => { console.error('FAILED:', e.message); process.exit(1); });
