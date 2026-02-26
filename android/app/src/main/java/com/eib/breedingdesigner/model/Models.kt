package com.eib.breedingdesigner.model

import kotlin.math.sqrt

// ─── Input model ──────────────────────────────────────────────────────────────

data class Stage(
    val entries: Int = 1000,
    val years: Int = 1,
    val locs: Int = 1,
    val reps: Int = 1,
    val errorVariance: Double = 1.0,
    val plotCost: Double = 10.0,
    val locCost: Double = 500.0,
    val fixedCost: Double = 0.0
) {
    fun heritability(varG: Double, varGxL: Double, varGxY: Double): Double {
        val denom = varG +
                varGxY / years +
                varGxL / (years.toDouble() * locs) +
                errorVariance / (years.toDouble() * locs * reps)
        return if (denom > 0) varG / denom else 0.0
    }

    fun cost(): Double =
        plotCost * entries * years * locs * reps +
                locCost * locs * years +
                fixedCost
}

data class Scenario(
    val id: Int,
    val name: String = "Scenario $id",
    val varG: Double = 1.0,
    val varGxL: Double = 0.5,
    val varGxY: Double = 0.5,
    val multYears: Int = 3,
    val stages: List<Stage> = defaultStages(),
    val varieties: Int = 5,
    val nRepeats: Int = 200
) {
    companion object {
        fun defaultStages() = listOf(
            Stage(entries = 1000, years = 1, locs = 1, reps = 1, errorVariance = 1.0),
            Stage(entries = 100,  years = 1, locs = 4, reps = 2, errorVariance = 1.0),
            Stage(entries = 10,   years = 2, locs = 8, reps = 3, errorVariance = 1.0)
        )
    }

    val totalYears: Int get() = multYears + stages.sumOf { it.years }
    val totalCost: Double get() = stages.sumOf { it.cost() }
}

data class SimulationParams(
    val varG: Double,
    val varGxL: Double,
    val varGxY: Double,
    val entries: List<Int>,
    val years: List<Int>,
    val locs: List<Int>,
    val reps: List<Int>,
    val error: List<Double>,
    val varieties: Int,
    val nRepeats: Int,
    val multYears: Int,
    // cost info for metrics
    val plotCosts: List<Double> = List(entries.size) { 10.0 },
    val locCosts: List<Double> = List(entries.size) { 500.0 },
    val fixedCosts: List<Double> = List(entries.size) { 0.0 }
) {
    fun totalCost(): Double = (entries.indices).sumOf { i ->
        plotCosts[i] * entries[i] * years[i] * locs[i] * reps[i] +
                locCosts[i] * locs[i] * years[i] +
                fixedCosts[i]
    }
}

fun Scenario.toParams() = SimulationParams(
    varG = varG, varGxL = varGxL, varGxY = varGxY,
    entries = stages.map { it.entries },
    years   = stages.map { it.years },
    locs    = stages.map { it.locs },
    reps    = stages.map { it.reps },
    error   = stages.map { it.errorVariance },
    varieties = varieties, nRepeats = nRepeats, multYears = multYears,
    plotCosts  = stages.map { it.plotCost },
    locCosts   = stages.map { it.locCost },
    fixedCosts = stages.map { it.fixedCost }
)

// ─── Result model ─────────────────────────────────────────────────────────────

data class StageStats(
    val mean: Double,
    val sd: Double,
    val median: Double,
    val q25: Double,
    val q75: Double,
    val min: Double,
    val max: Double
)

data class SimulationResult(
    val stageStats: List<StageStats>,
    val totalGain: Double,
    val gainPerYear: Double,
    val gainPerCost: Double,
    val rawOutput: Array<DoubleArray>
) {
    // Generated equals/hashCode to handle Array<DoubleArray>
    override fun equals(other: Any?) = other is SimulationResult &&
            stageStats == other.stageStats
    override fun hashCode() = stageStats.hashCode()
}

data class ScenarioResult(
    val scenario: Scenario,
    val result: SimulationResult
)
