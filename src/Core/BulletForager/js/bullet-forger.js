class BulletForger {
  constructor() {
    this.currentBullet = this.createDefaultBullet();
    this.templates = this.initializeTemplates();
    this.history = [];
    this.blockProgram = [];
    this.bulletLibrary = [];
    this.loadBulletFromLibrary();
    this.initializeUI();
  }

  createDefaultBullet() {
    return {
      name: 'New Bullet',
      color: '#ff00ff',
      damage: 50,
      bulletSpeed: 10,
      fireRate: 100,
      bulletSize: { w: 4, h: 6 },
      spread: 0,
      rayCount: 1,
      piercing: false,
      freeze: false,
      freezeDuration: 1000,
      burn: false,
      burnDamage: 0,
      poison: false,
      poisonDuration: 1000,
      trail: false,
      trailLength: 10,
      pull: false,
      pullRadius: 50,
      push: false,
      pushForce: 10,
      split: false,
      splitCount: 2,
      bounce: false,
      bounceCount: 3,
      homing: false,
      homingRadius: 100,
      track: false,
      trackingDistance: 150,
      explode: false,
      explosionRadius: 50,
      explosionDamage: 30,
      gravityAffect: 1,
      friction: 0.99,
      lifetime: 5000,
      ricochet: false,
      richochetCount: 2,
      slowMotion: false,
      slowFactor: 0.5,
      timeWarp: false,
      warpFactor: 1,
      phaseShift: false,
      phaseDistance: 30,
      magneticPull: false,
      magneticRange: 100,
      electricChain: false,
      chainLength: 3,
      chainDistance: 50,
      stun: false,
      stunDuration: 500,
      blind: false,
      blindDuration: 1000,
      weakPoint: false,
      weakMultiplier: 2,
      armor: false,
      armorReduction: 0.5,
      reflect: false,
      reflectChance: 0.3,
      absorb: false,
      absorbHeal: 5,
      spawnOnHit: false,
      spawnType: 'normal',
      spawnCount: 1,
      spawnDelay: 100,
      soundEffect: 'default',
      particleEffect: 'default',
      afterImage: false,
      afterImageCount: 3,
      afterImageDelay: 50,
      crit: false,
      critChance: 0.1,
      critMultiplier: 2,
      bleed: false,
      bleedDamage: 10,
      bleedDuration: 3000,
      silence: false,
      silenceDuration: 1000,
      confusion: false,
      confusionDuration: 2000,
      charm: false,
      charmDuration: 2000,
      cursed: false,
      cursedDebuff: 'damage',
      blessed: false,
      blessedBuff: 'defense',
      corrupted: false,
      corruptedPower: 20,
      purified: false,
      purifiedHeal: 10,
      infused: false,
      infusionType: 'fire',
      infusionPower: 25,
      primed: false,
      primedDelay: 500,
      chainReaction: false,
      reactionPower: 15,
      unstable: false,
      instability: 0.2,
      volatile: false,
      volatileThreshold: 80,
      reactive: false,
      reactiveCondition: 'hit',
      nullify: false,
      nullifyPower: 0.5,
      amplify: false,
      amplifyFactor: 1.5,
      diminish: false,
      diminishFactor: 0.7,
      negate: false,
      negatePower: 1,
      invert: false,
      invertType: 'damage',
      transcend: false,
      transcendPower: 100,
      absolute: false,
      absoluteZero: false,
      frostIntensity: 1,
      cosmicRay: false,
      cosmicDamage: 50,
      wormhole: false,
      wormholeStability: 0.8,
      realityWarp: false,
      warpIntensity: 1,
      existencePhage: false,
      phageDamage: 100,
      infinityLoop: false,
      loopIterations: 10,
      parallelWorld: false,
      parallelOffset: 20,
      darkMatter: false,
      darkMatterMass: 1,
      hawkingRadiation: false,
      hawkingEmission: 50,
      universalCollapse: false,
      collapseRadius: 200,
      voidRift: false,
      voidDamage: 150,
      entanglement: false,
      entanglementRange: 100,
      breakApart: false,
      breakDamageReduction: 0.3,
      necromancy: false,
      necromancyPower: 50,
      celestialBody: false,
      celestialRadius: 30,
      omniSlash: false,
      omniCount: 8,
      vorpalEdge: false,
      instakill: false,
      mirage: false,
      mirageCount: 3,
      timebomb: false,
      timebombDelay: 2000,
      starlight: false,
      starlightIntensity: 0.5,
      abyssalTent: false,
      abyssalReach: 100,
      molecularDisassemble: false,
      disassembleRate: 0.1,
      goldenRatio: false,
      goldenDamage: 75,
      plague: false,
      plagueSpread: 5,
      stormborn: false,
      stormintensity: 1,
      phantomBlade: false,
      phantomCount: 2,
      singularityPulse: false,
      pulseForce: 100,
      echoChamber: false,
      echoMultiplier: 2,
      mythrill: false,
      mythrilHardness: 10,
      divineSmite: false,
      divineBonus: 50,
      rift: false,
      riftDepth: 50,
      solarFlare: false,
      flareHeat: 100,
      lunarShade: false,
      shadowPower: 50,
      twilight: false,
      twilightBalance: 0.5,
      obliteration: false,
      obliterationRadius: 150,
      fragmentation: false,
      fragmentCount: 5,
      hyperspace: false,
      hyperspaceWarp: 0.8,
      apocalypse: false,
      apocalypseRadius: 500,
      rebirth: false,
      rebirthDamage: 200,
      sacrifice: false,
      sacrificeGain: 50,
      vengeance: false,
      vengeanceMultiplier: 3,
      legacy: false,
      legacyDuration: 5000,
      eternity: false,
      eternityLoop: true,
      breakChain: false,
      chaosEffectClass: 'chaos-spin'
    };
  }

  initializeTemplates() {
    return {
      fireBlast: {
        name: 'Fire Blast',
        color: '#ff4500',
        damage: 100,
        bulletSpeed: 12,
        fireRate: 120,
        burn: true,
        burnDamage: 30,
        explode: true,
        explosionRadius: 60,
        explosionDamage: 50,
        trail: true,
        trailLength: 15
      },
      iceShower: {
        name: 'Ice Shower',
        color: '#00d4ff',
        damage: 80,
        bulletSpeed: 10,
        fireRate: 140,
        freeze: true,
        freezeDuration: 2000,
        spread: 30,
        rayCount: 8,
        trail: true
      },
      poisonDart: {
        name: 'Poison Dart',
        color: '#00ff00',
        damage: 40,
        bulletSpeed: 15,
        fireRate: 100,
        poison: true,
        poisonDuration: 3000,
        bleed: true,
        bleedDamage: 5,
        bleedDuration: 2000,
        piercing: true
      },
      lightningBolt: {
        name: 'Lightning Bolt',
        color: '#ffff00',
        damage: 120,
        bulletSpeed: 20,
        fireRate: 150,
        electricChain: true,
        chainLength: 5,
        chainDistance: 100,
        stun: true,
        stunDuration: 500
      },
      holyWrath: {
        name: 'Holy Wrath',
        color: '#ffffff',
        damage: 150,
        bulletSpeed: 12,
        fireRate: 200,
        blessed: true,
        blessedBuff: 'defense',
        divineSmite: true,
        divineBonus: 75,
        explode: true,
        explosionRadius: 80
      },
      shadowBolt: {
        name: 'Shadow Bolt',
        color: '#1a0033',
        damage: 110,
        bulletSpeed: 14,
        fireRate: 130,
        lunarShade: true,
        shadowPower: 60,
        silence: true,
        silenceDuration: 1500,
        cursed: true,
        cursedDebuff: 'damage'
      }
    };
  }

  initializeUI() {
    const container = document.getElementById('bullet-forger-container') || this.createContainer();
    container.innerHTML = `
      <div class="forger-header">
        <h1>Bullet Forger</h1>
      </div>
      <div class="forger-main">
        <div class="forger-col" id="forger-col-1"></div>
        <div class="forger-col" id="forger-col-2"></div>
      </div>
    `;

    const col1 = document.getElementById('forger-col-1');
    const col2 = document.getElementById('forger-col-2');

    this.createMainWorkflow(col1);
    this.createPresetPanel(col1);
    this.createBlockCoder(col1);
    this.createParameterPanel(col2);
    this.createCodeEditor(col2);
  }

  createMainWorkflow(container) {
    const workflowPanel = document.createElement('div');
    workflowPanel.className = 'forger-panel workflow-panel';
    workflowPanel.innerHTML = `
      <h2>Workshop</h2>
      <div class="workflow-buttons">
        <button class="forger-btn" data-action="import">Import</button>
        <button class="forger-btn" data-action="download">Download</button>
        <button class="forger-btn" data-action="save">Save</button>
        <button class="forger-btn" data-action="export">Export</button>
        <button class="forger-btn" data-action="use-from-game">Use from Game</button>
        <button class="forger-btn" data-action="start-template">Template</button>
        <button class="forger-btn" data-action="remix">Remix</button>
        <button class="forger-btn" data-action="custom-scratch">Scratch</button>
        <button class="forger-btn" data-action="customize-abilities">Abilities</button>
        <button class="forger-btn" data-action="customize-speed">Speed</button>
      </div>
    `;

    container.appendChild(workflowPanel);
    this.attachWorkflowEvents(workflowPanel);
  }

  attachWorkflowEvents(panel) {
    const buttons = panel.querySelectorAll('.forger-btn');
    buttons.forEach(btn => {
      btn.addEventListener('click', (e) => {
        const action = e.target.dataset.action;
        this.handleWorkflowAction(action);
      });
    });
  }

  handleWorkflowAction(action) {
    switch (action) {
      case 'import':
        this.showImportDialog();
        break;
      case 'download':
        this.downloadBullet();
        break;
      case 'save':
        this.saveBulletToLibrary();
        break;
      case 'export':
        this.exportBullet();
        break;
      case 'use-from-game':
        this.loadFromGameBullets();
        break;
      case 'start-template':
        this.showTemplateSelector();
        break;
      case 'remix':
        this.remixBullet();
        break;
      case 'custom-scratch':
        this.startFromScratch();
        break;
      case 'customize-abilities':
        this.showAbilitiesPanel();
        break;
      case 'customize-speed':
        this.showSpeedPanel();
        break;
    }
  }

  createPresetPanel(container) {
    const presetPanel = document.createElement('div');
    presetPanel.className = 'forger-panel preset-panel';
    presetPanel.id = 'preset-panel';

    let html = '<h3>Presets</h3><div class="preset-buttons">';
    for (const key in weaponPresets) {
      const preset = weaponPresets[key];
      html += `<button class="forger-btn preset-btn" data-preset="${key}">${preset.name}</button>`;
    }
    html += '</div>';

    presetPanel.innerHTML = html;
    container.appendChild(presetPanel);
    this.attachPresetEvents(presetPanel);
  }

  attachPresetEvents(panel) {
    const buttons = panel.querySelectorAll('.preset-btn');
    buttons.forEach(btn => {
      btn.addEventListener('click', (e) => {
        const presetKey = e.target.dataset.preset;
        this.loadPreset(weaponPresets[presetKey]);
      });
    });
  }

  loadPreset(presetData) {
    this.currentBullet = { ...this.createDefaultBullet(), ...presetData };
    this.updateParameterPanel();
    this.updatePreview();
  }

  updateParameterPanel() {
      const oldPanel = document.getElementById('parameter-panel');
      if (oldPanel) {
          oldPanel.remove();
      }
      this.createParameterPanel(document.getElementById('forger-col-2'));
  }

  createParameterPanel(container) {
    const paramPanel = document.createElement('div');
    paramPanel.className = 'forger-panel parameter-panel';
    paramPanel.id = 'parameter-panel';

    const categories = this.getParameterCategories();
    let html = '<h3>Parameters</h3><div class="param-categories">';

    for (const [category, params] of Object.entries(categories)) {
      html += `<div class="param-category">
        <h4>${category}</h4>
        <div class="param-inputs">`;

      params.forEach(param => {
        const value = this.currentBullet[param.key] || '';
        const inputId = `param-${param.key}`;

        if (param.type === 'dimensions') {
          html += `
            <div class="param-item">
              <label for="${inputId}-w">${param.label}</label>
              <div class="param-input-group">
                <input type="number" id="${inputId}-w" data-param-part="w" data-param="bulletSize" value="${value.w}">
                <input type="number" id="${inputId}-h" data-param-part="h" data-param="bulletSize" value="${value.h}">
              </div>
            </div>`;
        } else if (param.type === 'number') {
          html += `
            <div class="param-item">
              <label for="${inputId}">${param.label}</label>
              <input type="number" id="${inputId}" data-param="${param.key}" value="${value}" min="${param.min}" max="${param.max}" step="${param.step}">
            </div>`;
        } else if (param.type === 'color') {
          html += `
            <div class="param-item">
              <label for="${inputId}">${param.label}</label>
              <input type="color" id="${inputId}" data-param="${param.key}" value="${value}">
            </div>`;
        } else if (param.type === 'checkbox') {
          html += `
            <div class="param-item param-item-checkbox">
              <input type="checkbox" id="${inputId}" data-param="${param.key}" ${value ? 'checked' : ''}>
              <label for="${inputId}">${param.label}</label>
            </div>`;
        } else if (param.type === 'select') {
          html += `
            <div class="param-item">
              <label for="${inputId}">${param.label}</label>
              <select id="${inputId}" data-param="${param.key}">`;
          param.options.forEach(opt => {
            html += `<option value="${opt}" ${value === opt ? 'selected' : ''}>${opt}</option>`;
          });
          html += `</select></div>`;
        }
      });

      html += '</div></div>';
    }

    html += '</div>';
    paramPanel.innerHTML = html;
    container.appendChild(paramPanel);
    this.attachParameterEvents();
  }

  attachParameterEvents() {
    const paramPanel = document.getElementById('parameter-panel');
    const inputs = paramPanel.querySelectorAll('input, select');

    inputs.forEach(input => {
      input.addEventListener('change', (e) => {
        const param = e.target.dataset.param;
        const paramPart = e.target.dataset.paramPart;
        let value = e.target.value;

        if (e.target.type === 'checkbox') {
          value = e.target.checked;
        } else if (e.target.type === 'number') {
          value = parseFloat(value);
        }
        if (paramPart) {
          this.currentBullet[param][paramPart] = value;
        }
        else {
          this.currentBullet[param] = value;
        }
        this.updatePreview();
      });
    });
  }

getParameterCategories() {
    return {
    'Core Ballistics': [
        { key: 'name', label: 'Bullet Name', type: 'text' },
        { key: 'color', label: 'Color', type: 'color' },
        { key: 'damage', label: 'Damage', type: 'number', min: 0, max: 500, step: 1 },
        { key: 'bulletSpeed', label: 'Speed', type: 'number', min: 0, max: 50, step: 0.5 },
        { key: 'fireRate', label: 'Fire Rate', type: 'number', min: 10, max: 500, step: 1 },
        { key: 'bulletSize', label: 'Size (W x H)', type: 'dimensions' }
    ],
    'Spread & Patterns': [
        { key: 'spread', label: 'Spread Angle', type: 'number', min: 0, max: 360, step: 1 },
        { key: 'rayCount', label: 'Ray Count', type: 'number', min: 1, max: 20, step: 1 },
        { key: 'bounceCount', label: 'Bounce Count', type: 'number', min: 0, max: 10, step: 1 },
        { key: 'ricochetCount', label: 'Ricochet Count', type: 'number', min: 0, max: 5, step: 1 },
        { key: 'phaseDistance', label: 'Phase Distance', type: 'number', min: 0, max: 100, step: 5 },
        { key: 'magneticRange', label: 'Magnetic Range', type: 'number', min: 0, max: 200, step: 10 },
        { key: 'split', label: 'Split', type: 'checkbox' },
    ],
    'Effects - Damage': [
        { key: 'burn', label: 'Burn', type: 'checkbox' },
        { key: 'burnDamage', label: 'Burn Damage/s', type: 'number', min: 0, max: 100, step: 1 },
        { key: 'poison', label: 'Poison', type: 'checkbox' },
        { key: 'poisonDuration', label: 'Poison Duration (ms)', type: 'number', min: 0, max: 5000, step: 100 },
        { key: 'bleed', label: 'Bleed', type: 'checkbox' },
        { key: 'bleedDamage', label: 'Bleed Damage', type: 'number', min: 0, max: 50, step: 1 }
    ],
    'Effects - Control': [
        { key: 'freeze', label: 'Freeze', type: 'checkbox' },
        { key: 'freezeDuration', label: 'Freeze Duration (ms)', type: 'number', min: 0, max: 5000, step: 100 },
        { key: 'stun', label: 'Stun', type: 'checkbox' },
        { key: 'stunDuration', label: 'Stun Duration (ms)', type: 'number', min: 0, max: 2000, step: 100 },
        { key: 'pull', label: 'Pull', type: 'checkbox' },
        { key: 'pullRadius', label: 'Pull Radius', type: 'number', min: 0, max: 200, step: 5 }
      ],
      'Effects - Special': [
        { key: 'explode', label: 'Explode on Hit', type: 'checkbox' },
        { key: 'explosionRadius', label: 'Explosion Radius', type: 'number', min: 0, max: 200, step: 5 },
        { key: 'homing', label: 'Homing', type: 'checkbox' },
        { key: 'homingRadius', label: 'Homing Radius', type: 'number', min: 0, max: 300, step: 10 },
        { key: 'crit', label: 'Critical Hit', type: 'checkbox' },
        { key: 'critChance', label: 'Crit Chance', type: 'number', min: 0, max: 1, step: 0.05 }
      ],
      'Physics': [
        { key: 'gravityAffect', label: 'Gravity Multiplier', type: 'number', min: 0, max: 2, step: 0.1 },
        { key: 'friction', label: 'Friction', type: 'number', min: 0, max: 1, step: 0.01 },
        { key: 'lifetime', label: 'Lifetime (ms)', type: 'number', min: 100, max: 10000, step: 100 }
      ],
      'Advanced': [
        { key: 'electricChain', label: 'Electric Chain', type: 'checkbox' },
        { key: 'chainLength', label: 'Chain Length', type: 'number', min: 1, max: 10, step: 1 },
        { key: 'divineSmite', label: 'Divine Smite', type: 'checkbox' },
        { key: 'divineBonus', label: 'Divine Bonus', type: 'number', min: 0, max: 200, step: 10 },
        { key: 'chaos', label: 'Chaos Effect', type: 'checkbox' }
      ]
    };
  }

  createCodeEditor(container) {
    const editorPanel = document.createElement('div');
    editorPanel.className = 'forger-panel code-editor-panel';
    editorPanel.id = 'code-editor';

    editorPanel.innerHTML = `
      <h3>Code Editor</h3>
      <div class="editor-toolbar">
        <button class="forger-btn" id="add-line">Add Line</button>
        <button class="forger-btn" id="delete-line">Delete Line</button>
        <button class="forger-btn" id="apply-code">Apply</button>
        <button class="forger-btn" id="validate-code">Validate</button>
      </div>
      <textarea id="bullet-code" class="code-textarea" placeholder="const bullet = {\n  name: 'My Bullet',\n  damage: 50\n};"></textarea>
      <div id="code-errors" class="code-errors"></div>
    `;

    container.appendChild(editorPanel);
    this.attachCodeEditorEvents();
  }

  attachCodeEditorEvents() {
    const applyBtn = document.getElementById('apply-code');
    const validateBtn = document.getElementById('validate-code');
    const addLineBtn = document.getElementById('add-line');
    const deleteLineBtn = document.getElementById('delete-line');
    const codeArea = document.getElementById('bullet-code');

    applyBtn?.addEventListener('click', () => this.applyCode());
    validateBtn?.addEventListener('click', () => this.validateCode());
    addLineBtn?.addEventListener('click', () => this.addCodeLine());
    deleteLineBtn?.addEventListener('click', () => this.deleteCodeLine());
  }

  applyCode() {
    const codeArea = document.getElementById('bullet-code');
    const code = codeArea.value;
    const errorDiv = document.getElementById('code-errors');

    try {
      const bullet = eval(`(${code})`);
      this.currentBullet = { ...this.currentBullet, ...bullet };
      errorDiv.textContent = '✓ Code applied successfully';
      errorDiv.style.color = 'green';
      this.updatePreview();
    } catch (e) {
      errorDiv.textContent = `Error: ${e.message}`;
      errorDiv.style.color = 'red';
    }
  }

  validateCode() {
    const codeArea = document.getElementById('bullet-code');
    const code = codeArea.value;
    const errorDiv = document.getElementById('code-errors');

    try {
      eval(`(${code})`);
      errorDiv.textContent = '✓ Code is valid';
      errorDiv.style.color = 'green';
    } catch (e) {
      errorDiv.textContent = `Invalid: ${e.message}`;
      errorDiv.style.color = 'red';
    }
  }

  addCodeLine() {
    const codeArea = document.getElementById('bullet-code');
    codeArea.value += '\n// New line';
  }

  deleteCodeLine() {
    const codeArea = document.getElementById('bullet-code');
    const lines = codeArea.value.split('\n');
    lines.pop();
    codeArea.value = lines.join('\n');
  }

  createBlockCoder(container) {
    const blockPanel = document.createElement('div');
    blockPanel.className = 'forger-panel block-coder-panel';
    blockPanel.id = 'block-coder';

    blockPanel.innerHTML = `
      <h3>Block Coder</h3>
      <div class="block-toolbar">
        <button class="forger-btn" data-block="damage">Damage</button>
        <button class="forger-btn" data-block="effect">Effect</button>
        <button class="forger-btn" data-block="physics">Physics</button>
        <button class="forger-btn" data-block="spread">Spread</button>
        <button class="forger-btn" data-block="trigger">Trigger</button>
        <button class="forger-btn" data-block="combo">Combo</button>
      </div>
      <div class="block-canvas" id="block-canvas">
        <p>Drag blocks here</p>
      </div>
      <button class="forger-btn" id="compile-blocks">Compile Blocks</button>
    `;

    container.appendChild(blockPanel);
    this.attachBlockCoderEvents();
  }

  attachBlockCoderEvents() {
    const blockBtns = document.querySelectorAll('.block-btn[data-block]');
    const compileBtn = document.getElementById('compile-blocks');

    blockBtns.forEach(btn => {
      btn.addEventListener('click', (e) => {
        const blockType = e.target.dataset.block;
        this.addBlockToCanvas(blockType);
      });
    });

    compileBtn?.addEventListener('click', () => this.compileBlocks());
  }

  addBlockToCanvas(blockType) {
    const canvas = document.getElementById('block-canvas');
    const block = document.createElement('div');
    block.className = 'block';
    block.draggable = true;

    const blockConfigs = {
      damage: { label: 'Damage', params: ['amount', 'type'] },
      effect: { label: 'Effect', params: ['burn', 'freeze', 'poison'] },
      physics: { label: 'Physics', params: ['gravity', 'friction', 'bounce'] },
      spread: { label: 'Spread', params: ['angle', 'rayCount'] },
      trigger: { label: 'Trigger', params: ['onHit', 'onExplode'] },
      combo: { label: 'Combo', params: ['chain1', 'chain2'] }
    };

    const config = blockConfigs[blockType];
    block.innerHTML = `
      <div class="block-header">${config.label}</div>
      <div class="block-params">
        ${config.params.map(p => `<div class="param">${p}: <input type="text"></div>`).join('')}
      </div>
      <button class="block-delete">✕</button>
    `;

    block.addEventListener('dragstart', (e) => {
      e.dataTransfer.effectAllowed = 'move';
    });

    block.querySelector('.block-delete')?.addEventListener('click', () => block.remove());

    canvas.appendChild(block);
  }

  compileBlocks() {
    const blocks = document.querySelectorAll('.block');
    const config = {};

    blocks.forEach(block => {
      const header = block.querySelector('.block-header').textContent;
      const params = block.querySelectorAll('.param input');
      params.forEach(input => {
        config[input.previousSibling.textContent.split(':')[0]] = input.value;
      });
    });

    this.currentBullet = { ...this.currentBullet, ...config };
    this.updatePreview();
  }

  showImportDialog() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.json';
    input.addEventListener('change', (e) => {
      const file = e.target.files[0];
      const reader = new FileReader();
      reader.onload = (event) => {
        try {
          const imported = JSON.parse(event.target.result);
          this.currentBullet = { ...this.currentBullet, ...imported };
          this.updatePreview();
        } catch (err) {
          alert('Invalid JSON file: ' + err.message);
        }
      };
      reader.readAsText(file);
    });
    input.click();
  }

  downloadBullet() {
    const dataStr = JSON.stringify(this.currentBullet, null, 2);
    const dataBlob = new Blob([dataStr], { type: 'application/json' });
    const url = URL.createObjectURL(dataBlob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${this.currentBullet.name || 'bullet'}.json`;
    link.click();
    URL.revokeObjectURL(url);
  }

  saveBulletToLibrary() {
    const name = prompt("Enter a name for your bullet:", this.currentBullet.name);
    if (name) {
      this.currentBullet.name = name;
      const existingIndex = this.bulletLibrary.findIndex(b => b.name === name);
      if (existingIndex > -1) {
        this.bulletLibrary[existingIndex] = { ...this.currentBullet };
      } else {
        this.bulletLibrary.push({ ...this.currentBullet });
      }
      localStorage.setItem('bulletLibrary', JSON.stringify(this.bulletLibrary));
      alert(`✓ Saved: ${this.currentBullet.name}`);
    }
  }

  loadBulletFromLibrary() {
    const savedBullets = JSON.parse(localStorage.getItem('bulletLibrary') || '[]');
    this.bulletLibrary = savedBullets;
  }

  exportBullet() {
    const code = `const bulletConfig = ${JSON.stringify(this.currentBullet, null, 2)};`;
    const blob = new Blob([code], { type: 'text/javascript' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${this.currentBullet.name}-config.js`;
    link.click();
    URL.revokeObjectURL(url);
  }

  loadFromGameBullets() {
    const preset = confirm('Load preset bullet from game? (Select OK for random, Cancel to choose)');
    if (preset) {
      const presets = Object.values(this.templates);
      this.currentBullet = { ...presets[Math.floor(Math.random() * presets.length)] };
    } else {
      this.showTemplateSelector();
    }
    this.updatePreview();
  }

  showTemplateSelector() {
    const dialog = document.createElement('div');
    dialog.className = 'template-dialog';
    dialog.innerHTML = '<h3>Select Template</h3><div class="template-list">';

    for (const [key, template] of Object.entries(this.templates)) {
      dialog.querySelector('.template-list').innerHTML += `
        <button class="template-option" data-template="${key}" style="background: ${template.color}">
          ${template.name}
        </button>
      `;
    }

    document.body.appendChild(dialog);

    dialog.querySelectorAll('.template-option').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const key = e.target.dataset.template;
        this.currentBullet = { ...this.templates[key] };
        dialog.remove();
        this.updatePreview();
      });
    });
  }

  remixBullet() {
    const props = ['damage', 'bulletSpeed', 'burnDamage', 'explosionDamage', 'bleedDamage'];
    props.forEach(prop => {
      if (typeof this.currentBullet[prop] === 'number') {
        this.currentBullet[prop] = Math.floor(this.currentBullet[prop] * (0.8 + Math.random() * 0.4));
      }
    });
    this.updatePreview();
  }

  startFromScratch() {
    this.currentBullet = this.createDefaultBullet();
    this.updatePreview();
    alert('✓ Started fresh bullet');
  }

  showAbilitiesPanel() {
    const abilities = ['burn', 'freeze', 'poison', 'stun', 'explode', 'homing', 'electricChain', 'bleed', 'crit', 'divineSmite'];
    const dialog = document.createElement('div');
    dialog.className = 'abilities-dialog';
    dialog.innerHTML = '<h3>Bullet Abilities</h3><div class="ability-list">';

    abilities.forEach(ability => {
      const enabled = this.currentBullet[ability];
      dialog.querySelector('.ability-list').innerHTML += `
        <label>
          <input type="checkbox" data-ability="${ability}" ${enabled ? 'checked' : ''}>
          ${ability.charAt(0).toUpperCase() + ability.slice(1)}
        </label>
      `;
    });

    dialog.innerHTML += '<button class="confirm-btn">✓ Apply</button>';
    document.body.appendChild(dialog);

    dialog.querySelector('.confirm-btn').addEventListener('click', () => {
      dialog.querySelectorAll('input[type="checkbox"]').forEach(checkbox => {
        this.currentBullet[checkbox.dataset.ability] = checkbox.checked;
      });
      dialog.remove();
      this.updatePreview();
    });
  }

  showSpeedPanel() {
    const speedOptions = [5, 10, 15, 20, 25, 30];
    const dialog = document.createElement('div');
    dialog.className = 'speed-dialog';
    dialog.innerHTML = '<h3>Bullet Speed</h3><div class="speed-list">';

    speedOptions.forEach(speed => {
      dialog.querySelector('.speed-list').innerHTML += `
        <button class="speed-option" data-speed="${speed}">
          Speed ${speed}
        </button>
      `;
    });

    document.body.appendChild(dialog);

    dialog.querySelectorAll('.speed-option').forEach(btn => {
      btn.addEventListener('click', (e) => {
        this.currentBullet.bulletSpeed = parseFloat(e.target.dataset.speed);
        dialog.remove();
        this.updatePreview();
      });
    });
  }

  updatePreview() {
    const codeArea = document.getElementById('bullet-code');
    if (codeArea) {
      codeArea.value = JSON.stringify(this.currentBullet, null, 2);
    }
  }

  createContainer() {
    const container = document.createElement('div');
    container.id = 'bullet-forger-container';
    container.className = 'bullet-forger-container';
    document.body.appendChild(container);
    return container;
  }

  exportBulletToGame() {
    return JSON.stringify(this.currentBullet);
  }

  getCurrentBullet() {
    return this.currentBullet;
  }
}

const forger = new BulletForger();
