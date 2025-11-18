// CloudSkanr Web Interface

let regions = [];
let rules = [];
let sightings = [];

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
    loadStatus();
    loadRegions();
    loadRules();
    loadSightings();

    // Auto-refresh every 10 seconds
    setInterval(() => {
        loadStatus();
        loadSightings();
    }, 10000);
});

// API Calls
async function apiCall(endpoint, method = 'GET', data = null) {
    const options = {
        method,
        headers: {
            'Content-Type': 'application/json'
        }
    };

    if (data) {
        options.body = JSON.stringify(data);
    }

    const response = await fetch(endpoint, options);
    return response.json();
}

// Status
async function loadStatus() {
    try {
        const status = await apiCall('/api/status');
        document.getElementById('statusText').textContent =
            status.is_active ? 'SYSTEM ACTIVE' : 'SYSTEM STANDBY';
    } catch (error) {
        console.error('Failed to load status:', error);
        document.getElementById('statusText').textContent = 'CONNECTION ERROR';
    }
}

// Regions
async function loadRegions() {
    try {
        regions = await apiCall('/api/regions');
        renderRegions();
        updateStats();
        updateRuleRegionSelect();
    } catch (error) {
        console.error('Failed to load regions:', error);
    }
}

function renderRegions() {
    const tbody = document.getElementById('regionsTable');
    tbody.innerHTML = regions.map(region => `
        <tr>
            <td>${region.name}</td>
            <td class="coordinate">${region.lat.toFixed(3)}, ${region.lon.toFixed(3)}</td>
            <td>${region.radius_miles} mi</td>
            <td>
                <span class="badge ${region.active ? 'badge-active' : 'badge-inactive'}">
                    ${region.active ? 'ACTIVE' : 'INACTIVE'}
                </span>
            </td>
            <td>
                <button class="btn btn-danger" onclick="deleteRegion('${region.id}')">Delete</button>
            </td>
        </tr>
    `).join('');
}

function openRegionModal() {
    document.getElementById('regionForm').reset();
    document.getElementById('regionModal').style.display = 'flex';
}

function closeRegionModal() {
    document.getElementById('regionModal').style.display = 'none';
}

async function saveRegion(event) {
    event.preventDefault();

    const region = {
        id: document.getElementById('regionId').value,
        name: document.getElementById('regionName').value,
        lat: parseFloat(document.getElementById('regionLat').value),
        lon: parseFloat(document.getElementById('regionLon').value),
        radius_miles: parseFloat(document.getElementById('regionRadius').value),
        active: document.getElementById('regionActive').value === 'true'
    };

    try {
        await apiCall('/api/regions', 'POST', region);
        closeRegionModal();
        loadRegions();
    } catch (error) {
        console.error('Failed to save region:', error);
        alert('Failed to save region');
    }
}

async function deleteRegion(id) {
    if (!confirm(`Delete region ${id}?`)) return;

    try {
        await apiCall(`/api/regions/${id}`, 'DELETE');
        loadRegions();
    } catch (error) {
        console.error('Failed to delete region:', error);
    }
}

// Rules
async function loadRules() {
    try {
        rules = await apiCall('/api/rules');
        renderRules();
        updateStats();
    } catch (error) {
        console.error('Failed to load rules:', error);
    }
}

function renderRules() {
    const tbody = document.getElementById('rulesTable');
    tbody.innerHTML = rules.map(rule => {
        const types = rule.aircraft_types === 'any' ? 'ANY' : rule.aircraft_types.join(', ');
        return `
            <tr>
                <td>${rule.region_id}</td>
                <td>${types}</td>
                <td>${rule.min_altitude_ft}-${rule.max_altitude_ft} ft</td>
                <td>${rule.max_distance_miles} mi</td>
                <td>
                    <span class="badge badge-priority">P${rule.priority}</span>
                </td>
                <td>
                    <button class="btn btn-danger" onclick="deleteRule(${rule.id})">Delete</button>
                </td>
            </tr>
        `;
    }).join('');
}

function updateRuleRegionSelect() {
    const select = document.getElementById('ruleRegion');
    select.innerHTML = regions.map(region =>
        `<option value="${region.id}">${region.name}</option>`
    ).join('');
}

function openRuleModal() {
    document.getElementById('ruleForm').reset();
    document.getElementById('ruleModal').style.display = 'flex';
}

function closeRuleModal() {
    document.getElementById('ruleModal').style.display = 'none';
}

async function saveRule(event) {
    event.preventDefault();

    const typesInput = document.getElementById('ruleTypes').value.trim();
    let aircraft_types;

    if (typesInput.toLowerCase() === 'any') {
        aircraft_types = 'any';
    } else {
        aircraft_types = typesInput.split(',').map(t => t.trim()).filter(t => t);
    }

    const rule = {
        region_id: document.getElementById('ruleRegion').value,
        aircraft_types: aircraft_types,
        min_altitude_ft: parseInt(document.getElementById('ruleMinAlt').value),
        max_altitude_ft: parseInt(document.getElementById('ruleMaxAlt').value),
        max_distance_miles: parseFloat(document.getElementById('ruleMaxDist').value),
        priority: parseInt(document.getElementById('rulePriority').value)
    };

    try {
        await apiCall('/api/rules', 'POST', rule);
        closeRuleModal();
        loadRules();
    } catch (error) {
        console.error('Failed to save rule:', error);
        alert('Failed to save rule');
    }
}

async function deleteRule(id) {
    if (!confirm(`Delete rule #${id}?`)) return;

    try {
        await apiCall(`/api/rules/${id}`, 'DELETE');
        loadRules();
    } catch (error) {
        console.error('Failed to delete rule:', error);
    }
}

// Sightings
async function loadSightings() {
    try {
        sightings = await apiCall('/api/sightings?limit=50');
        renderSightings();
        updateStats();
    } catch (error) {
        console.error('Failed to load sightings:', error);
    }
}

function renderSightings() {
    const container = document.getElementById('sightingsContainer');

    if (sightings.length === 0) {
        container.innerHTML = '<p style="color: var(--text-dim); text-align: center; padding: 2rem;">No recent sightings</p>';
        return;
    }

    container.innerHTML = sightings.map(sighting => {
        const timestamp = new Date(sighting.timestamp * 1000).toLocaleString();
        const notifiedClass = sighting.notified ? 'sighting-notified' : '';

        return `
            <div class="sighting-item ${notifiedClass}">
                <div>
                    <strong>${sighting.callsign || sighting.icao24}</strong><br>
                    <span class="timestamp">${timestamp}</span>
                </div>
                <div>
                    Region: ${sighting.region_id}<br>
                    <span class="coordinate">${sighting.lat.toFixed(3)}, ${sighting.lon.toFixed(3)}</span>
                </div>
                <div>
                    Alt: ${sighting.altitude_ft} ft<br>
                    Hdg: ${sighting.heading}°
                </div>
                <div>
                    ${sighting.notified ?
                        '<span class="badge badge-priority">ALERTED</span>' :
                        '<span class="badge badge-inactive">TRACKED</span>'
                    }
                </div>
            </div>
        `;
    }).join('');
}

// Stats
function updateStats() {
    const activeRegions = regions.filter(r => r.active).length;
    document.getElementById('regionCount').textContent = activeRegions;
    document.getElementById('ruleCount').textContent = rules.length;
    document.getElementById('sightingCount').textContent = sightings.length;
}
