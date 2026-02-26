package com.eib.breedingdesigner.simulation

import com.eib.breedingdesigner.model.*
import org.junit.Assert.*
import org.junit.Test

/**
 * JVM unit tests for RangesSimulator and supporting helpers.
 */
class RangesSimulatorTest {

    // ── rangeGrain helper ─────────────────────────────────────────────────────

    @Test
    fun `rangeGrain returns correct endpoints`() {
        val result = RangesSimulator.rangeGrain(100, 1000, 3)
        assertEquals(100,  result.first())
        assertEquals(1000, result.last())
    }

    @Test
    fun `rangeGrain with samples=2 returns only min and max`() {
        val result = RangesSimulator.rangeGrain(1, 5, 2)
        assertEquals(listOf(1, 5), result)
    }

    @Test
    fun `rangeGrain with samples=3 returns evenly spaced values`() {
        val result = RangesSimulator.rangeGrain(10, 20, 3)
        assertEquals(3, result.size)
        assertEquals(10, result[0])
        assertEquals(15, result[1])
        assertEquals(20, result[2])
    }

    @Test
    fun `rangeGrain with equal min and max returns single value`() {
        val result = RangesSimulator.rangeGrain(5, 5, 3)
        assertEquals(1, result.size)
        assertEquals(5, result[0])
    }

    @Test
    fun `rangeGrain produces monotonically increasing sequence`() {
        val result = RangesSimulator.rangeGrain(10, 100, 5)
        for (i in 1 until result.size) {
            assertTrue("Sequence must be increasing", result[i] >= result[i - 1])
        }
    }

    // ── RangesConfig validation ───────────────────────────────────────────────

    @Test(expected = IllegalArgumentException::class)
    fun `RangeParam rejects max less than min`() {
        RangeParam(min = 100, max = 50)  // should throw
    }

    @Test(expected = IllegalArgumentException::class)
    fun `RangeParam rejects samples less than 2`() {
        RangeParam(min = 1, max = 10, samples = 1)  // should throw
    }

    @Test
    fun `RangeParam accepts valid configuration`() {
        val p = RangeParam(min = 100, max = 1000, samples = 5)
        assertEquals(100,  p.min)
        assertEquals(1000, p.max)
        assertEquals(5,    p.samples)
    }

    // ── RangesSimulator.run ───────────────────────────────────────────────────

    private fun minimalConfig() = RangesConfig(
        entriesRange = RangeParam(200, 500, 2),
        yearsRange   = RangeParam(1,   2,   2),
        locsRange    = RangeParam(1,   2,   2),
        repsRange    = RangeParam(1,   2,   2),
        varG = 1.0, varGxL = 0.5, varGxY = 0.5,
        errorVariance = 1.0,
        varieties = 1,
        nRepeats  = 50,
        multYears = 2,
        fixedStages = listOf(
            Stage(entries = 20, years = 1, locs = 4, reps = 2),
            Stage(entries = 5,  years = 2, locs = 8, reps = 3)
        )
    )

    @Test
    fun `run produces a non-empty result`() {
        val result = RangesSimulator.run(minimalConfig())
        assertTrue("Expected at least one result point", result.points.isNotEmpty())
    }

    @Test
    fun `run skips combinations where first-stage entries is not greater than next stage`() {
        // fixedStages.first().entries = 20, so entriesRange min must be > 20
        val cfg = minimalConfig().copy(
            entriesRange = RangeParam(10, 30, 3)  // 10 and 20 are invalid (≤ 20)
        )
        val result = RangesSimulator.run(cfg)
        result.points.forEach { pt ->
            assertTrue(
                "First-stage entries (${pt.entries}) must exceed next-stage entries (20)",
                pt.entries > 20
            )
        }
    }

    @Test
    fun `result contains correct axis value lists`() {
        val cfg = minimalConfig()
        val result = RangesSimulator.run(cfg)
        // yearsValues should contain [1, 2]
        assertEquals(listOf(1, 2), result.yearsValues)
        // locsValues should contain [1, 2]
        assertEquals(listOf(1, 2), result.locsValues)
    }

    @Test
    fun `all result points have positive mean gain`() {
        val result = RangesSimulator.run(minimalConfig())
        result.points.forEach { pt ->
            assertTrue(
                "Expected positive gain for (E=${pt.entries},Y=${pt.years},L=${pt.locs},R=${pt.reps}), got ${pt.meanGain}",
                pt.meanGain > 0.0
            )
        }
    }

    @Test
    fun `gain per year is positive for all points`() {
        val result = RangesSimulator.run(minimalConfig())
        result.points.forEach { pt ->
            assertTrue("gainPerYear > 0", pt.gainPerYear > 0.0)
        }
    }

    // ── RangeResult helpers ───────────────────────────────────────────────────

    @Test
    fun `heatmapData returns expected number of points`() {
        val result = RangesSimulator.run(minimalConfig())
        // With samples=2 for entries (filtered by nextStage constraint) and years=2
        val hm = result.heatmapData(RangeAxis.ENTRIES, RangeAxis.YEARS)
        // Each (x, y) pair should appear once
        assertEquals(hm.size, hm.keys.toSet().size)
    }

    @Test
    fun `lineData groups points by treatment axis`() {
        val result = RangesSimulator.run(minimalConfig())
        val lineData = result.lineData(RangeAxis.ENTRIES, RangeAxis.YEARS)
        // Should have one entry per unique YEARS value
        assertEquals(result.yearsValues.size, lineData.size)
    }

    @Test
    fun `axisValues returns correct list for each axis`() {
        val result = RangesSimulator.run(minimalConfig())
        assertEquals(result.entriesValues, result.axisValues(RangeAxis.ENTRIES))
        assertEquals(result.yearsValues,   result.axisValues(RangeAxis.YEARS))
        assertEquals(result.locsValues,    result.axisValues(RangeAxis.LOCS))
        assertEquals(result.repsValues,    result.axisValues(RangeAxis.REPS))
    }

    @Test
    fun `defaultFixed returns values for the two non-selected axes`() {
        val result = RangesSimulator.run(minimalConfig())
        val fixed  = result.defaultFixed(RangeAxis.ENTRIES, RangeAxis.YEARS)
        assertTrue("Should contain LOCS", fixed.containsKey(RangeAxis.LOCS))
        assertTrue("Should contain REPS", fixed.containsKey(RangeAxis.REPS))
        assertFalse("Should NOT contain ENTRIES", fixed.containsKey(RangeAxis.ENTRIES))
        assertFalse("Should NOT contain YEARS",   fixed.containsKey(RangeAxis.YEARS))
    }

    // ── getValue extension ────────────────────────────────────────────────────

    @Test
    fun `getValue returns correct field per axis`() {
        val pt = RangePoint(entries = 100, years = 2, locs = 4, reps = 3,
            meanGain = 0.5, sdGain = 0.1, gainPerYear = 0.1, gainPerCost = 0.05)
        assertEquals(100, pt.getValue(RangeAxis.ENTRIES))
        assertEquals(2,   pt.getValue(RangeAxis.YEARS))
        assertEquals(4,   pt.getValue(RangeAxis.LOCS))
        assertEquals(3,   pt.getValue(RangeAxis.REPS))
    }
}
