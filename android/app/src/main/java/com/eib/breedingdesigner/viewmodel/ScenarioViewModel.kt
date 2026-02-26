package com.eib.breedingdesigner.viewmodel

import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewModelScope
import com.eib.breedingdesigner.model.*
import com.eib.breedingdesigner.simulation.BreedingSimulator
import com.eib.breedingdesigner.simulation.RangesSimulator
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

data class AppState(
    val scenarios: List<Scenario> = listOf(Scenario(id = 1)),
    val activeScenarioId: Int = 1,
    val results: Map<Int, ScenarioResult> = emptyMap(),
    val isRunning: Boolean = false,
    val errorMessage: String? = null
)

class ScenarioViewModel : ViewModel() {

    private val _state = MutableStateFlow(AppState())
    val state: StateFlow<AppState> = _state.asStateFlow()

    private var nextId = 2

    // ─── Scenario management ──────────────────────────────────────────────────

    fun addScenario() {
        val newId = nextId++
        _state.update { s ->
            val base = s.scenarios.firstOrNull { it.id == s.activeScenarioId }
                ?: s.scenarios.last()
            s.copy(
                scenarios = s.scenarios + base.copy(id = newId, name = "Scenario $newId"),
                activeScenarioId = newId
            )
        }
    }

    fun removeScenario(id: Int) {
        _state.update { s ->
            val updated = s.scenarios.filter { it.id != id }
            val newActive = if (s.activeScenarioId == id) updated.lastOrNull()?.id ?: 1
                            else s.activeScenarioId
            s.copy(
                scenarios = updated.ifEmpty { listOf(Scenario(id = 1)) },
                activeScenarioId = newActive,
                results = s.results - id
            )
        }
    }

    fun setActive(id: Int) = _state.update { it.copy(activeScenarioId = id) }

    // ─── Scenario editing ─────────────────────────────────────────────────────

    fun updateScenario(updated: Scenario) = _state.update { s ->
        s.copy(scenarios = s.scenarios.map { if (it.id == updated.id) updated else it })
    }

    fun updateVarG(id: Int, v: Double) = editScenario(id) { it.copy(varG = v) }
    fun updateVarGxL(id: Int, v: Double) = editScenario(id) { it.copy(varGxL = v) }
    fun updateVarGxY(id: Int, v: Double) = editScenario(id) { it.copy(varGxY = v) }
    fun updateMultYears(id: Int, v: Int) = editScenario(id) { it.copy(multYears = v) }
    fun updateVarieties(id: Int, v: Int) = editScenario(id) { it.copy(varieties = v) }
    fun updateNRepeats(id: Int, v: Int) = editScenario(id) { it.copy(nRepeats = v) }
    fun updateName(id: Int, v: String) = editScenario(id) { it.copy(name = v) }

    fun addStage(id: Int) = editScenario(id) { s ->
        val last = s.stages.last()
        val newStage = last.copy(
            entries = maxOf(1, last.entries / 10),
            years = 1, locs = last.locs, reps = last.reps
        )
        s.copy(stages = s.stages + newStage)
    }

    fun removeStage(id: Int) = editScenario(id) { s ->
        if (s.stages.size > 1) s.copy(stages = s.stages.dropLast(1)) else s
    }

    fun updateStage(scenarioId: Int, stageIndex: Int, stage: Stage) =
        editScenario(scenarioId) { s ->
            val updated = s.stages.toMutableList().also { it[stageIndex] = stage }
            s.copy(stages = updated)
        }

    private fun editScenario(id: Int, transform: (Scenario) -> Scenario) =
        _state.update { s ->
            s.copy(scenarios = s.scenarios.map { if (it.id == id) transform(it) else it })
        }

    // ─── Simulation ───────────────────────────────────────────────────────────

    fun runSimulation(scenarioId: Int) {
        val scenario = _state.value.scenarios.firstOrNull { it.id == scenarioId } ?: return
        if (!validateScenario(scenario)) return

        _state.update { it.copy(isRunning = true, errorMessage = null) }

        viewModelScope.launch {
            val result = withContext(Dispatchers.Default) {
                BreedingSimulator.run(scenario.toParams())
            }
            _state.update { s ->
                s.copy(
                    isRunning = false,
                    results = s.results + (scenarioId to ScenarioResult(scenario, result))
                )
            }
        }
    }

    fun runAll() {
        _state.value.scenarios.forEach { runSimulation(it.id) }
    }

    fun clearError() = _state.update { it.copy(errorMessage = null) }

    // ─── Ranges ───────────────────────────────────────────────────────────────

    private val _rangesState = MutableStateFlow(RangesState())
    val rangesState: StateFlow<RangesState> = _rangesState.asStateFlow()

    fun updateRangesConfig(cfg: RangesConfig) = _rangesState.update { it.copy(config = cfg) }
    fun updateRangesXAxis(axis: RangeAxis)    = _rangesState.update { it.copy(xAxis = axis) }
    fun updateRangesYAxis(axis: RangeAxis)    = _rangesState.update { it.copy(yAxis = axis) }
    fun updateRangesXLine(axis: RangeAxis)    = _rangesState.update { it.copy(xAxisLine = axis) }
    fun updateRangesTreatment(axis: RangeAxis)= _rangesState.update { it.copy(treatmentAxis = axis) }

    fun updateRangesFixed(axis: RangeAxis, value: Int) = _rangesState.update { s ->
        s.copy(fixedValues = s.fixedValues + (axis to value))
    }

    fun runRanges() {
        val cfg = _rangesState.value.config
        _rangesState.update { it.copy(isRunning = true, errorMsg = null) }
        viewModelScope.launch {
            try {
                val result = withContext(Dispatchers.Default) { RangesSimulator.run(cfg) }
                _rangesState.update { s ->
                    // Reset fixed values to minimum of each axis after a new run
                    val fixed = s.defaultFixed(result, s.xAxis, s.yAxis)
                    s.copy(isRunning = false, result = result, fixedValues = fixed)
                }
            } catch (e: Exception) {
                _rangesState.update { it.copy(isRunning = false, errorMsg = e.message ?: "Unknown error") }
            }
        }
    }

    fun clearRangesError() = _rangesState.update { it.copy(errorMsg = null) }

    private fun RangesState.defaultFixed(
        result: RangeResult,
        xAxis: RangeAxis,
        yAxis: RangeAxis
    ): Map<RangeAxis, Int> = result.defaultFixed(xAxis, yAxis)

    // ─── Validation ───────────────────────────────────────────────────────────

    private fun validateScenario(s: Scenario): Boolean {
        // Entries must be strictly decreasing
        for (i in 1 until s.stages.size) {
            if (s.stages[i].entries >= s.stages[i - 1].entries) {
                _state.update { it.copy(errorMessage = "Entries must decrease across stages (stage ${i + 1} ≥ stage $i).") }
                return false
            }
        }
        if (s.varieties >= s.stages.last().entries) {
            _state.update { it.copy(errorMessage = "Final varieties must be less than last-stage entries.") }
            return false
        }
        return true
    }
}
