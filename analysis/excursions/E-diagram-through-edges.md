# Excursion: Dashed "Through" Edges for Compressed Diagrams

**Date:** 2026-04-13
**Entry point:** Manual testing of compressed diagram + adjacent non-content nodes

When a diagram is compressed, entities connected to its hidden contents
appear as free-floating neighbours with no visible connection to the
diagram node. Example: Nets → Corks (via scholium), but Corks is folded
inside Essay Ethics. Nets appears adjacent but disconnected.

**Desired behaviour:** A dashed line from Nets to the Essay Ethics
diagram node, indicating "this node connects to something inside."
The edge would be synthetic (not a real relation) and styled
differently (dashed, lighter colour) to distinguish from real edges.

**Implementation sketch:**
1. When filtering content nodes from a compressed diagram, collect
   all edges that connect a visible node to a hidden content node
2. For each such edge, create a synthetic edge from the visible node
   to the diagram node, with `:link/type "diagram/through"` and
   dashed styling
3. The synthetic edge carries the original edge's annotation as a
   tooltip or label prefix
