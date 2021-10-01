// Some examples from https://web.iitd.ac.in/~sumeet/slide3.pdf
// And https://www.cl.cam.ac.uk/archive/mjcg/TempLogic/Lectures/L4.Jan27.pdf
// And https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6073162,
// YIN Chongyuan (殷翀元) et al：Efficient Translation of LTL to Büchi Automata

// LTL ltl1: <> { rich }
// LTL ltl2: <> { x > 5 }
// LTL ltl3: [] ( {start} -> <> {terminate} )
// LTL ltl4: [] ( {trying} -> <> {critical} )
// LTL ltl5: G (!({started} /\ !{ready}))
// LTL ltl6: G ({requested} -> F{acknowledged})
// LTL ltl7: G (F {enabled})
// LTL ltl8: F (G {deadlock})
// LTL ltl9: G({AtFloor2} ∧ {DirectionUp} ∧ {RequestFloor5} ⇒ ({DirectionUp} U {AtFloor5}))
// LTL ltl10: {p1} U {p2}
// LTL ltl11: {p1} U ({p2} U {p3})
// LTL ltl12: ({p1} U {p2}) U {p3}
// LTL ltl13: G ( F {p} )
// LTL ltl14: G ( F {p1} ) -> G ( F {p2} )
// LTL ltl15: F {p1} U G {p2}
// LTL ltl16: G {p2} U {p2}
// LTL ltl17: (G {x})
// LTL ltl18: ! (G {x})

