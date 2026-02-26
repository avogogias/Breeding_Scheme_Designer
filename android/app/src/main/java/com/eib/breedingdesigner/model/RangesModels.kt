package com.eib.breedingdesigner.model

/**
 * Which first-stage parameter is being varied on a chart axis.
 */
enum class RangeAxis(val label: String) {
    ENTRIES("Entries"),
    YEARS("Years"),
    LOCS("Locations"),
    REPS("Reps/Loc")
}

/** Returns the value of a RangePoint field corresponding to the given axis. */
fun RangePoint.getValue(axis: RangeAxis): Int = when (axis) {
    RangeAxis.ENTRIES -> entries
    RangeAxis.YEARS   -> years
    RangeAxis.LOCS    -> locs
    RangeAxis.REPS    -> reps
}

// ─── Range parameter (min / max / number of samples) ──────────────────────────

data class RangeParam(
    val min: Int,
    val max: Int,
    val samples: Int = 3
) {
    init {
        require(min >= 1)   { "min must be ≥ 1" }
        require(max >= min) { "max must be ≥ min" }
        require(samples >= 2) { "samples must be ≥ 2" }
    }
}

// ─── Full ranges configuration ─────────────────────────────────────────────────

/**
 * Configuration for the Ranges exploration mode.
 *
 * Only the FIRST stage parameters are swept; remaining stages are kept fixed
 * (mirroring the behaviour of the original R Shiny Ranges tab).
 */
data class RangesConfig(
    val entriesRange: RangeParam = RangeParam(100, 1000, 3),
    val yearsRange:   RangeParam = RangeParam(1,   5,    3),
    val locsRange:    RangeParam = RangeParam(1,   5,    3),
    val repsRange:    RangeParam = RangeParam(1,   5,    3),

    // Variances
    val varG:   Double = 1.0,
    val varGxL: Double = 0.5,
    val varGxY: Double = 0.5,
    val errorVariance: Double = 1.0,

    // Costs (applied to first stage only – subsequent stages use their own values)
    val plotCost:  Double = 10.0,
    val locCost:   Double = 1000.0,
    val fixedCost: Double = 0.0,

    // Selection
    val varieties: Int = 1,
    val nRepeats:  Int = 100,
    val multYears: Int = 3,

    /** Fixed subsequent stages (entries must be < first-stage min). */
    val fixedStages: List<Stage> = listOf(
        Stage(entries = 50,  years = 1, locs = 4, reps = 2, errorVariance = 1.0,
              plotCost = 10.0, locCost = 1000.0),
        Stage(entries = 10,  years = 2, locs = 8, reps = 3, errorVariance = 1.0,
              plotCost = 10.0, locCost = 1000.0)
    )
)

// ─── Result types ──────────────────────────────────────────────────────────────

/**
 * A single data point produced by one (entries, years, locs, reps) combination.
 */
data class RangePoint(
    val entries:     Int,
    val years:       Int,
    val locs:        Int,
    val reps:        Int,
    val meanGain:    Double,
    val sdGain:      Double,
    val gainPerYear: Double,
    val gainPerCost: Double
)

/**
 * Aggregate of all simulation runs for a RangesConfig.
 * Stores the raw points and the unique values for each axis (for chart building).
 */
data class RangeResult(
    val config:        RangesConfig,
    val points:        List<RangePoint>,
    val entriesValues: List<Int>,
    val yearsValues:   List<Int>,
    val locsValues:    List<Int>,
    val repsValues:    List<Int>
) {
    /** All unique values for the requested axis. */
    fun axisValues(axis: RangeAxis): List<Int> = when (axis) {
        RangeAxis.ENTRIES -> entriesValues
        RangeAxis.YEARS   -> yearsValues
        RangeAxis.LOCS    -> locsValues
        RangeAxis.REPS    -> repsValues
    }

    /**
     * Filter points to only those where all axes NOT in [xAxis, yAxis]
     * match [fixedValues], then pivot into a map (xVal, yVal) → RangePoint.
     */
    fun heatmapData(
        xAxis:       RangeAxis,
        yAxis:       RangeAxis,
        fixedValues: Map<RangeAxis, Int> = emptyMap()
    ): Map<Pair<Int, Int>, RangePoint> {
        val fixed = fixedValues.filterKeys { it != xAxis && it != yAxis }
        return points
            .filter { pt -> fixed.all { (ax, v) -> pt.getValue(ax) == v } }
            .associateBy { pt -> Pair(pt.getValue(xAxis), pt.getValue(yAxis)) }
    }

    /**
     * Returns lists of (xValue → meanGain) keyed by the treatment-axis value.
     * Fixed axes beyond xAxis and treatmentAxis use [fixedValues].
     */
    fun lineData(
        xAxis:         RangeAxis,
        treatmentAxis: RangeAxis,
        fixedValues:   Map<RangeAxis, Int> = emptyMap()
    ): Map<Int, List<Pair<Int, Double>>> {
        val fixed = fixedValues.filterKeys { it != xAxis && it != treatmentAxis }
        val filtered = points.filter { pt -> fixed.all { (ax, v) -> pt.getValue(ax) == v } }
        return filtered
            .groupBy { it.getValue(treatmentAxis) }
            .mapValues { (_, pts) ->
                pts.sortedBy { it.getValue(xAxis) }
                   .map { Pair(it.getValue(xAxis), it.meanGain) }
            }
    }

    /** Default fixed values (minimum of each remaining axis). */
    fun defaultFixed(xAxis: RangeAxis, yAxis: RangeAxis): Map<RangeAxis, Int> {
        val remaining = RangeAxis.entries.filter { it != xAxis && it != yAxis }
        return remaining.associateWith { ax -> axisValues(ax).first() }
    }
}

// ─── ViewModel state ───────────────────────────────────────────────────────────

data class RangesState(
    val config:      RangesConfig = RangesConfig(),
    val result:      RangeResult? = null,
    val isRunning:   Boolean = false,
    val errorMsg:    String? = null,
    val xAxis:       RangeAxis = RangeAxis.ENTRIES,
    val yAxis:       RangeAxis = RangeAxis.REPS,
    val xAxisLine:   RangeAxis = RangeAxis.ENTRIES,
    val treatmentAxis: RangeAxis = RangeAxis.YEARS,
    val fixedValues: Map<RangeAxis, Int> = emptyMap()
)
