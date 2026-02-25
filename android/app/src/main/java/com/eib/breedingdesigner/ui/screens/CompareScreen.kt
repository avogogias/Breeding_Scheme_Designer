package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.ui.components.CompareChart
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
        // Toolbar
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
            Box(Modifier.fillMaxWidth().height(180.dp), contentAlignment = Alignment.Center) {
                Text(
                    "Run at least one simulation to compare scenarios.",
                    textAlign = TextAlign.Center,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
        } else {
            // Comparison chart
            Card(Modifier.fillMaxWidth(), elevation = CardDefaults.cardElevation(2.dp)) {
                Column(Modifier.padding(16.dp)) {
                    CompareChart(results, Modifier.fillMaxWidth())
                }
            }

            // Scenario cards
            results.forEach { sr ->
                CompareScenarioCard(
                    sr = sr,
                    isActive = state.activeScenarioId == sr.scenario.id,
                    onSelect = { vm.setActive(sr.scenario.id) },
                    onDelete = { vm.removeScenario(sr.scenario.id) }
                )
            }
        }
    }
}

@Composable
private fun CompareScenarioCard(
    sr: com.eib.breedingdesigner.model.ScenarioResult,
    isActive: Boolean,
    onSelect: () -> Unit,
    onDelete: () -> Unit
) {
    val borderColor = if (isActive) MaterialTheme.colorScheme.primary else Color.Transparent
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
                    "Stages: ${sr.result.stageStats.size}",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
            IconButton(onClick = onDelete) {
                Icon(Icons.Default.Delete, contentDescription = "Delete", tint = MaterialTheme.colorScheme.error)
            }
        }
    }
}
