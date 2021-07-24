// Some examples from https://web.iitd.ac.in/~sumeet/slide3.pdf
// And https://www.cl.cam.ac.uk/archive/mjcg/TempLogic/Lectures/L4.Jan27.pdf
// And https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6073162,
// YIN Chongyuan (殷翀元) et al：Efficient Translation of LTL to Büchi Automata

// LTL: <> { rich }
// LTL: <> { x > 5 }
// LTL: [] ( {start} -> <> {terminate} )
// LTL: [] ( {trying} -> <> {critical} )
// LTL: G (!({started} /\ !{ready}))
// LTL: G ({requested} -> F{acknowledged})
// LTL: G (F {enabled})
// LTL: F (G {deadlock})
// LTL: G({AtFloor2} ∧ {DirectionUp} ∧ {RequestFloor5} ⇒ ({DirectionUp} U {AtFloor5}))

// LTL: {p1} U {p2}
// LTL: {p1} U ({p2} U {p3})
// LTL: ({p1} U {p2}) U {p3}
// LTL: G ( F {p} )
// LTL: G ( F {p1} ) -> G ( F {p2} )
// LTL: F {p1} U G {p2}
// LTL: G {p2} U {p2}

