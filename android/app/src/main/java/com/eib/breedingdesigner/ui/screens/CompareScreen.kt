package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.model.ScenarioResult
import com.eib.breedingdesigner.ui.components.CompareChart
import com.eib.breedingdesigner.ui.components.GroupMetric
import com.eib.breedingdesigner.ui.components.GroupedBoxPlotChart
import com.eib.breedingdesigner.viewmodel.ScenarioViewModel

@Composable
fun CompareScreen(vm: ScenarioViewModel) {
    val state by vm.state.collectAsState()
    val results = state.results.values.toList()

    Column(
        modifier = Modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp)
    ) {
        // Header
        Row(
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier.fillMaxWidth()
        ) {
            Text(
                "Compare Scenarios",
                style = MaterialTheme.typography.headlineSmall,
                modifier = Modifier.weight(1f)
            )
            if (state.scenarios.size < 5) {
                IconButton(onClick = { vm.addScenario() }) {
                    Icon(Icons.Default.Add, contentDescription = "Add scenario")
                }
            }
        }

        if (results.isEmpty()) {
            Box(
                Modifier
                    .fillMaxWidth()
                    .height(180.dp),
                contentAlignment = Alignment.Center
            ) {
                Text(
                    "Run at least one simulation to compare scenarios.",
                    textAlign = TextAlign.Center,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
        } else {
            CompareContent(results, vm, state.activeScenarioId)
        }
    }
}

// ─── Main content ─────────────────────────────────────────────────────────────

@Composable
private fun CompareContent(
    results: List<ScenarioResult>,
    vm: ScenarioViewModel,
    activeId: Int
) {
    var selectedTab by remember { mutableStateOf(0) }

    // Tab selector
    TabRow(selectedTabIndex = selectedTab) {
        Tab(
            selected = selectedTab == 0,
            onClick = { selectedTab = 0 },
            text = { Text("Overview") }
        )
        Tab(
            selected = selectedTab == 1,
            onClick = { selectedTab = 1 },
            text = { Text("Trends") }
        )
    }

    Spacer(Modifier.height(8.dp))

    when (selectedTab) {
        0 -> OverviewTab(results)
        1 -> TrendsTab(results)
    }

    // Scenario management cards (always visible below tabs)
    Spacer(Modifier.height(4.dp))
    Text(
        "Scenarios",
        style = MaterialTheme.typography.titleSmall,
        modifier = Modifier.padding(bottom = 4.dp)
    )
    results.forEach { sr ->
        CompareScenarioCard(
            sr = sr,
            isActive = activeId == sr.scenario.id,
            onSelect = { vm.setActive(sr.scenario.id) },
            onDelete = { vm.removeScenario(sr.scenario.id) }
        )
    }
}

// ─── Overview tab – grouped box plots (mirrors R's Overview tab) ──────────────

@Composable
private fun OverviewTab(results: List<ScenarioResult>) {
    Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
        GroupMetric.entries.forEach { metric ->
            Card(
                modifier = Modifier.fillMaxWidth(),
                elevation = CardDefaults.cardElevation(2.dp)
            ) {
                GroupedBoxPlotChart(
                    results = results,
                    metric = metric,
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp)
                )
            }
        }
    }
}

// ─── Trends tab – median line chart ──────────────────────────────────────────

@Composable
private fun TrendsTab(results: List<ScenarioResult>) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        CompareChart(results, Modifier.fillMaxWidth().padding(16.dp))
    }
}

// ─── Scenario card ────────────────────────────────────────────────────────────

@Composable
private fun CompareScenarioCard(
    sr: ScenarioResult,
    isActive: Boolean,
    onSelect: () -> Unit,
    onDelete: () -> Unit
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(
            containerColor = if (isActive) MaterialTheme.colorScheme.primaryContainer
                             else MaterialTheme.colorScheme.surface
        ),
        elevation = CardDefaults.cardElevation(if (isActive) 4.dp else 1.dp),
        onClick = onSelect
    ) {
        Row(
            Modifier.padding(16.dp),
            verticalAlignment = Alignment.CenterVertically
        ) {
            Column(Modifier.weight(1f)) {
                Text(sr.scenario.name, style = MaterialTheme.typography.titleSmall)
                Text(
                    "Gain: ${"%.4f".format(sr.result.totalGain)}  " +
                    "/yr: ${"%.4f".format(sr.result.gainPerYear)}  " +
                    "/cost: ${"%.2e".format(sr.result.gainPerCost)}  " +
                    "Stages: ${sr.result.stageStats.size}",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
            IconButton(onClick = onDelete) {
                Icon(
                    Icons.Default.Delete,
                    contentDescription = "Delete",
                    tint = MaterialTheme.colorScheme.error
                )
            }
        }
    }
}
