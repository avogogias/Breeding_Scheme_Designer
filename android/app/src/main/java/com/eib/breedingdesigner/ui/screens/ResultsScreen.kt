package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.model.ScenarioResult
import com.eib.breedingdesigner.ui.components.BoxPlotChart
import com.eib.breedingdesigner.viewmodel.ScenarioViewModel

@Composable
fun ResultsScreen(
    vm: ScenarioViewModel,
    scenarioId: Int
) {
    val state by vm.state.collectAsState()
    val scenarioResult = state.results[scenarioId]
    val isRunning = state.isRunning

    Column(
        modifier = Modifier
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp)
    ) {
        if (isRunning) {
            Box(Modifier.fillMaxWidth().height(200.dp), contentAlignment = Alignment.Center) {
                Column(horizontalAlignment = Alignment.CenterHorizontally) {
                    CircularProgressIndicator()
                    Spacer(Modifier.height(16.dp))
                    Text("Running simulation…", style = MaterialTheme.typography.bodyMedium)
                }
            }
        } else if (scenarioResult == null) {
            Box(Modifier.fillMaxWidth().height(200.dp), contentAlignment = Alignment.Center) {
                Text(
                    "Configure your breeding scheme in Setup\nthen press Run Simulation.",
                    style = MaterialTheme.typography.bodyLarge,
                    textAlign = TextAlign.Center,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
            }
        } else {
            ResultContent(scenarioResult)
        }
    }
}

@Composable
private fun ResultContent(sr: ScenarioResult) {
    // Box plot card
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        Column(Modifier.padding(16.dp)) {
            BoxPlotChart(sr, Modifier.fillMaxWidth())
        }
    }

    // Summary metrics card
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
            Text("Summary Metrics", style = MaterialTheme.typography.titleMedium, color = MaterialTheme.colorScheme.primary)
            Divider()
            MetricRow("Total Genetic Gain", "%.4f".format(sr.result.totalGain))
            MetricRow("Gain per Year", "%.4f".format(sr.result.gainPerYear))
            MetricRow("Gain per \$1000", "%.4f".format(sr.result.gainPerCost * 1000))
            MetricRow("Total Program Years", "${sr.scenario.totalYears}")
            MetricRow("Total Program Cost", "\$${"%.0f".format(sr.scenario.totalCost)}")
        }
    }

    // Per-stage table
    Card(
        modifier = Modifier.fillMaxWidth(),
        elevation = CardDefaults.cardElevation(2.dp)
    ) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(4.dp)) {
            Text("Stage Statistics", style = MaterialTheme.typography.titleMedium, color = MaterialTheme.colorScheme.primary)
            Divider()
            // Header
            Row(Modifier.fillMaxWidth()) {
                listOf("Stage", "Mean", "SD", "Q25", "Q75").forEach { hdr ->
                    Text(hdr, style = MaterialTheme.typography.labelSmall, modifier = Modifier.weight(1f), textAlign = TextAlign.Center)
                }
            }
            sr.result.stageStats.forEachIndexed { i, st ->
                Divider(thickness = 0.5.dp)
                Row(Modifier.fillMaxWidth()) {
                    listOf(
                        "${i + 1}",
                        "%.3f".format(st.mean),
                        "%.3f".format(st.sd),
                        "%.3f".format(st.q25),
                        "%.3f".format(st.q75)
                    ).forEach { cell ->
                        Text(cell, style = MaterialTheme.typography.bodySmall, modifier = Modifier.weight(1f), textAlign = TextAlign.Center)
                    }
                }
            }
        }
    }
}

@Composable
private fun MetricRow(label: String, value: String) {
    Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
        Text(label, style = MaterialTheme.typography.bodyMedium)
        Text(value, style = MaterialTheme.typography.bodyMedium, color = MaterialTheme.colorScheme.primary)
    }
}
