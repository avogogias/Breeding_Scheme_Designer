package com.eib.breedingdesigner.simulation

import com.eib.breedingdesigner.model.*
import org.junit.Assert.*
import org.junit.Test
import kotlin.math.abs

/**
 * JVM unit tests for BreedingSimulator (no Android dependencies).
 *
 * Tests verify:
 *   1. Positive genetic gain when selection pressure is high.
 *   2. More stages → monotonically increasing gain across stages.
 *   3. Higher genetic variance → larger gain (relative).
 *   4. Gain increases when heritability is higher (more reps / locs).
 *   5. Single-stage scenario produces a result.
 *   6. Stage stats are internally consistent (q25 ≤ median ≤ q75, etc.).
 *   7. Determinism within a single run (result structure only – not seeded).
 *   8. Result dimensions match nStages.
 */
class BreedingSimulatorTest {

    private fun defaultParams(
        entries: List<Int>   = listOf(1000, 100, 10),
        years:   List<Int>   = listOf(1, 1, 2),
        locs:    List<Int>   = listOf(1, 4, 8),
        reps:    List<Int>   = listOf(1, 2, 3),
        error:   List<Double> = listOf(1.0, 1.0, 1.0),
        varG:    Double = 1.0,
        varGxL:  Double = 0.5,
        varGxY:  Double = 0.5,
        varieties: Int  = 5,
        nRepeats:  Int  = 200,
        multYears: Int  = 3
    ) = SimulationParams(
        varG = varG, varGxL = varGxL, varGxY = varGxY,
        entries = entries, years = years, locs = locs, reps = reps, error = error,
        varieties = varieties, nRepeats = nRepeats, multYears = multYears
    )

    // ── 1. Positive gain ─────────────────────────────────────────────────────

    @Test
    fun `final stage mean gain is positive under selection`() {
        val result = BreedingSimulator.run(defaultParams())
        assertTrue(
            "Expected positive final gain, got ${result.totalGain}",
            result.totalGain > 0.0
        )
    }

    // ── 2. Stage gain is monotonically non-decreasing ─────────────────────────

    @Test
    fun `mean gain increases monotonically across stages`() {
        val result = BreedingSimulator.run(defaultParams())
        val means = result.stageStats.map { it.mean }
        for (i in 1 until means.size) {
            assertTrue(
                "Gain should not decrease: stage $i (${means[i - 1]}) > stage ${i + 1} (${means[i]})",
                means[i] >= means[i - 1] - 1e-6  // small tolerance for rounding
            )
        }
    }

    // ── 3. Higher varG → larger gain ─────────────────────────────────────────

    @Test
    fun `higher genetic variance yields higher gain`() {
        val lowVarG  = BreedingSimulator.run(defaultParams(varG = 0.5)).totalGain
        val highVarG = BreedingSimulator.run(defaultParams(varG = 2.0)).totalGain
        assertTrue(
            "Higher varG should give higher gain (low=$lowVarG, high=$highVarG)",
            highVarG > lowVarG
        )
    }

    // ── 4. More replications → higher gain (better heritability) ─────────────

    @Test
    fun `more replications yields higher mean gain`() {
        val fewReps  = BreedingSimulator.run(defaultParams(reps = listOf(1, 1, 1))).totalGain
        val manyReps = BreedingSimulator.run(defaultParams(reps = listOf(5, 5, 5))).totalGain
        assertTrue(
            "More reps should give higher gain (few=$fewReps, many=$manyReps)",
            manyReps > fewReps
        )
    }

    // ── 5. Single-stage scenario ──────────────────────────────────────────────

    @Test
    fun `single stage scenario produces valid result`() {
        val params = defaultParams(
            entries   = listOf(100),
            years     = listOf(2),
            locs      = listOf(4),
            reps      = listOf(3),
            error     = listOf(1.0),
            varieties = 5
        )
        val result = BreedingSimulator.run(params)
        assertEquals(1, result.stageStats.size)
        assertTrue(result.totalGain > 0.0)
    }

    // ── 6. Stage stat internal consistency ───────────────────────────────────

    @Test
    fun `stage stats are internally consistent`() {
        val result = BreedingSimulator.run(defaultParams())
        result.stageStats.forEachIndexed { i, st ->
            assertTrue("min ≤ q25 at stage $i", st.min  <= st.q25  + 1e-9)
            assertTrue("q25 ≤ median at stage $i", st.q25 <= st.median + 1e-9)
            assertTrue("median ≤ q75 at stage $i", st.median <= st.q75 + 1e-9)
            assertTrue("q75 ≤ max at stage $i", st.q75  <= st.max  + 1e-9)
            assertTrue("sd ≥ 0 at stage $i", st.sd >= 0.0)
        }
    }

    // ── 7. Number of stage results matches stages input ──────────────────────

    @Test
    fun `result has same number of stage stats as input stages`() {
        val nStages = 4
        val params = defaultParams(
            entries = listOf(1000, 200, 40, 8),
            years   = listOf(1,    1,   1,  2),
            locs    = listOf(1,    2,   4,  8),
            reps    = listOf(1,    2,   2,  3),
            error   = listOf(1.0, 1.0, 1.0, 1.0),
            varieties = 2
        )
        val result = BreedingSimulator.run(params)
        assertEquals(nStages, result.stageStats.size)
    }

    // ── 8. Gain per year and gain per cost are positive ───────────────────────

    @Test
    fun `gain per year and gain per cost are positive`() {
        val result = BreedingSimulator.run(defaultParams())
        assertTrue("gainPerYear > 0", result.gainPerYear > 0.0)
        // gainPerCost may be 0 if costs are 0 (default plotCosts = List(n){10})
        // Just check it is non-negative
        assertTrue("gainPerCost >= 0", result.gainPerCost >= 0.0)
    }

    // ── 9. Zero genetic variance → near-zero gain ────────────────────────────

    @Test
    fun `zero genetic variance yields near-zero gain`() {
        val result = BreedingSimulator.run(defaultParams(varG = 0.0))
        // With no genetic variance there is nothing to select for
        assertTrue(
            "With varG=0 expected near-zero gain, got ${result.totalGain}",
            abs(result.totalGain) < 0.1
        )
    }

    // ── 10. Heritability formula (from Stage model) ───────────────────────────

    @Test
    fun `heritability is 1 when there are no non-genetic effects`() {
        val stage = Stage(entries = 100, years = 1, locs = 1, reps = 1, errorVariance = 0.0)
        val h2 = stage.heritability(varG = 1.0, varGxL = 0.0, varGxY = 0.0)
        assertEquals(1.0, h2, 1e-9)
    }

    @Test
    fun `heritability is between 0 and 1 for typical values`() {
        val stage = Stage(entries = 100, years = 1, locs = 4, reps = 2, errorVariance = 1.0)
        val h2 = stage.heritability(varG = 1.0, varGxL = 0.5, varGxY = 0.5)
        assertTrue("h2 should be in (0,1)", h2 > 0.0 && h2 < 1.0)
    }

    @Test
    fun `heritability increases with more reps`() {
        val stage1rep = Stage(entries = 100, years = 1, locs = 4, reps = 1, errorVariance = 1.0)
        val stage5rep = Stage(entries = 100, years = 1, locs = 4, reps = 5, errorVariance = 1.0)
        val h2low  = stage1rep.heritability(1.0, 0.5, 0.5)
        val h2high = stage5rep.heritability(1.0, 0.5, 0.5)
        assertTrue("More reps → higher h2 ($h2low < $h2high)", h2high > h2low)
    }
}
