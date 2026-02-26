package com.eib.breedingdesigner.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.eib.breedingdesigner.viewmodel.ScenarioViewModel

private sealed class Screen(val route: String, val label: String, val icon: ImageVector) {
    object Setup   : Screen("setup",   "Setup",   Icons.Default.Tune)
    object Results : Screen("results", "Results", Icons.Default.BarChart)
    object Ranges  : Screen("ranges",  "Ranges",  Icons.Default.GridView)
    object Compare : Screen("compare", "Compare", Icons.Default.CompareArrows)
    object Help    : Screen("help",    "Help",    Icons.Default.HelpOutline)
}

private val screens = listOf(
    Screen.Setup,
    Screen.Results,
    Screen.Ranges,
    Screen.Compare,
    Screen.Help
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MainScreen(vm: ScenarioViewModel = viewModel()) {
    val state by vm.state.collectAsState()
    var currentScreen by remember { mutableStateOf<Screen>(Screen.Setup) }

    val scenarios = state.scenarios

    Scaffold(
        topBar = {
            Column {
                TopAppBar(
                    title = { Text("Breeding Scheme Designer") },
                    colors = TopAppBarDefaults.topAppBarColors(
                        containerColor = MaterialTheme.colorScheme.primary,
                        titleContentColor = MaterialTheme.colorScheme.onPrimary
                    ),
                    actions = {
                        if (currentScreen == Screen.Setup && scenarios.size < 5) {
                            IconButton(onClick = { vm.addScenario() }) {
                                Icon(Icons.Default.Add, "Add scenario",
                                    tint = MaterialTheme.colorScheme.onPrimary)
                            }
                        }
                    }
                )
                // Scenario tabs (only shown in Setup / Results screens)
                if (scenarios.size > 1 &&
                    (currentScreen == Screen.Setup || currentScreen == Screen.Results)) {
                    ScrollableTabRow(
                        selectedTabIndex = scenarios
                            .indexOfFirst { it.id == state.activeScenarioId }
                            .coerceAtLeast(0),
                        edgePadding = 0.dp,
                        containerColor = MaterialTheme.colorScheme.primaryContainer
                    ) {
                        scenarios.forEach { sc ->
                            Tab(
                                selected = sc.id == state.activeScenarioId,
                                onClick  = { vm.setActive(sc.id) },
                                text     = { Text(sc.name, maxLines = 1) }
                            )
                        }
                    }
                }
            }
        },
        bottomBar = {
            NavigationBar {
                screens.forEach { screen ->
                    NavigationBarItem(
                        selected = currentScreen == screen,
                        onClick  = { currentScreen = screen },
                        icon     = { Icon(screen.icon, contentDescription = screen.label) },
                        label    = { Text(screen.label) }
                    )
                }
            }
        }
    ) { padding ->
        Box(Modifier.padding(padding).fillMaxSize()) {
            when (currentScreen) {
                Screen.Setup   -> SetupScreen(
                    vm         = vm,
                    scenarioId = state.activeScenarioId,
                    onRunClicked = {
                        vm.runSimulation(state.activeScenarioId)
                        currentScreen = Screen.Results
                    }
                )
                Screen.Results -> ResultsScreen(vm, state.activeScenarioId)
                Screen.Ranges  -> RangesScreen(vm)
                Screen.Compare -> CompareScreen(vm)
                Screen.Help    -> HelpScreen()
            }
        }
    }
}
