package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.PlayArrow
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.eib.breedingdesigner.model.Scenario
import com.eib.breedingdesigner.model.Stage
import com.eib.breedingdesigner.ui.components.*
import com.eib.breedingdesigner.viewmodel.ScenarioViewModel

@Composable
fun SetupScreen(
    vm: ScenarioViewModel,
    scenarioId: Int,
    onRunClicked: () -> Unit
) {
    val state by vm.state.collectAsState()
    val scenario = state.scenarios.firstOrNull { it.id == scenarioId } ?: return

    // Error snackbar
    state.errorMessage?.let { msg ->
        LaunchedEffect(msg) {
            vm.clearError()
        }
        Snackbar(
            modifier = Modifier.padding(16.dp),
            action = { TextButton(onClick = { vm.clearError() }) { Text("OK") } }
        ) { Text(msg) }
    }

    Box(Modifier.fillMaxSize()) {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .verticalScroll(rememberScrollState())
                .padding(start = 16.dp, end = 16.dp, top = 16.dp, bottom = 88.dp),
            verticalArrangement = Arrangement.spacedBy(16.dp)
        ) {
            // Scenario name
            OutlinedTextField(
                value = scenario.name,
                onValueChange = { vm.updateName(scenarioId, it) },
                label = { Text("Scenario Name") },
                singleLine = true,
                modifier = Modifier.fillMaxWidth()
            )

            // Variances card
            SectionCard(title = "Genetic Variances") {
                DoubleInput("Genetic Variance (σ²G)", scenario.varG, { vm.updateVarG(scenarioId, it) }, Modifier.fillMaxWidth())
                DoubleInput("GxL Variance (σ²GxL)", scenario.varGxL, { vm.updateVarGxL(scenarioId, it) }, Modifier.fillMaxWidth())
                DoubleInput("GxY Variance (σ²GxY)", scenario.varGxY, { vm.updateVarGxY(scenarioId, it) }, Modifier.fillMaxWidth())
            }

            // Selection parameters
            SectionCard(title = "Selection Parameters") {
                IntInput("Multiplication Years", scenario.multYears, { vm.updateMultYears(scenarioId, it) }, Modifier.fillMaxWidth(), min = 0)
                IntInput("Final Varieties Selected", scenario.varieties, { vm.updateVarieties(scenarioId, it) }, Modifier.fillMaxWidth(), min = 1)
                IntInput("Simulation Replicates", scenario.nRepeats, { vm.updateNRepeats(scenarioId, it) }, Modifier.fillMaxWidth(), min = 10, max = 2000)
            }

            // Stages
            SectionCard(title = "Yield Trial Stages") {
                StageEditorList(
                    scenario = scenario,
                    onStageChanged = { idx, stage -> vm.updateStage(scenarioId, idx, stage) },
                    onAddStage = { vm.addStage(scenarioId) },
                    onRemoveStage = { vm.removeStage(scenarioId) }
                )
            }

            // Cost summary
            CostSummaryCard(scenario)
        }

        // Run FAB
        ExtendedFloatingActionButton(
            onClick = onRunClicked,
            icon = { Icon(Icons.Default.PlayArrow, contentDescription = null) },
            text = { Text(if (state.isRunning) "Running…" else "Run Simulation") },
            containerColor = MaterialTheme.colorScheme.primary,
            modifier = Modifier
                .align(androidx.compose.ui.Alignment.BottomEnd)
                .padding(24.dp)
        )
    }
}

@Composable
fun CostSummaryCard(scenario: Scenario) {
    SectionCard(title = "Cost Summary") {
        scenario.stages.forEachIndexed { i, stage ->
            Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
                Text("Stage ${i + 1}", style = MaterialTheme.typography.bodyMedium)
                Text("$${"%.0f".format(stage.cost())}", style = MaterialTheme.typography.bodyMedium)
            }
        }
        Divider(Modifier.padding(vertical = 4.dp))
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Text("Total", style = MaterialTheme.typography.titleSmall)
            Text("$${"%.0f".format(scenario.totalCost)}", style = MaterialTheme.typography.titleSmall)
        }
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Text("Total Years", style = MaterialTheme.typography.bodySmall)
            Text("${scenario.totalYears}", style = MaterialTheme.typography.bodySmall)
        }
    }
}

@Composable
fun SectionCard(
    title: String,
    content: @Composable ColumnScope.() -> Unit
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.surface),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp)
    ) {
        Column(Modifier.padding(16.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
            Text(title, style = MaterialTheme.typography.titleMedium, color = MaterialTheme.colorScheme.primary)
            Divider()
            content()
        }
    }
}
