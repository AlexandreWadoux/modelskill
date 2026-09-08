# CRPS decomposition implementation record

The release implementation was written from the mathematical specification
in the function help, following Hersbach (2000), rather than retaining the
previous verification-derived routine. The previous implementation and upstream
source had been inspected during the audit; this is not a claim of a clean-room
development process or a legal certification. No upstream source is included
in the replacement or its tests, and verification is not a dependency.

The upstream routine was authored by Ronald Frenette (2009) and distributed
in verification under GPL (>= 2). Earlier implementations remain visible in
Git history for traceability, but are not included in the 0.1.0 source tarball.
Upstream source: https://github.com/cran/verification/blob/master/R/crpsDecomposition.r.

For each adjacent pair of sorted ensemble members, clipping the observation
to that interval gives its lengths below and above the observation. Their
case means are A and B. With W = A + B and nominal probability p = j/m,
the contribution to CRPS is A*p^2 + B*(1-p)^2. For W > 0, reliability is
W*(B/W-p)^2 and potential CRPS is A*(B/W). Zero-width bins contribute zero.

Exterior contributions use mean distance L below the smallest member and U
above the largest. Let qL be the proportion obs <= smallest and qU the
proportion obs > largest. The reliability contribution is L*qL + U*qU;
the potential contribution is L*(1-qL) + U*(1-qU). This follows by cancelling
the tail-frequency denominators in the documented equations. The previously
documented inclusive-CDF tie convention is retained.

The score is calculated from the integrated CDF loss, not by adding its two
components. Tests independently verify it with pairwise ensemble distances.
The component test oracle integrates threshold-indicator pieces using explicit
breakpoints and midpoints, rather than the production clipping operations.
Hand-calculated examples, tied members, zero-width bins, missing rows, invalid
inputs, random continuous/discrete ensembles and invariance properties are
also covered. No upstream implementation is used as the test oracle.

Reference: Hersbach, H. (2000). Decomposition of the continuous ranked
probability score for ensemble prediction systems. Weather and Forecasting,
15, 559-570. DOI: 10.1175/1520-0434(2000)015<0559:DOTCRP>2.0.CO;2.
