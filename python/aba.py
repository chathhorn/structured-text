from enum import Enum, auto

debug = True

class B(Enum):
      AND   = auto()
      OR    = auto()
      TRUE  = auto()
      FALSE = auto()
      TERM  = auto()

class BNode:
      def __init__(self, tag, left=None, right=None):
            self.tag = tag
            self.left = left
            self.right = right

      def __repr__(self):
            if self.tag == B.TERM:
                  return f'{self.left}'

            if self.tag == B.AND:
                  return f'({self.left} & {self.right})'

            if self.tag == B.OR:
                  return f'({self.left} | {self.right})'

            if self.tag == B.TRUE:
                  return f'true'

            if self.tag == B.FALSE:
                  return f'false'

      def __eq__(self, other):
            return type(other) == type(self) and self.tag == other.tag and self.left == other.left and self.right == other.right

      def __hash__(self):
            return hash(str(self))

def simplify(b):
      if type(b) is BNode:
            if b.tag == B.AND:
                  left = simplify(b.left)
                  right = simplify(b.right)
                  if left.tag == B.FALSE or right.tag == B.FALSE:
                        return BNode(B.FALSE)
                  if left.tag == B.TRUE:
                        return right
                  if right.tag == B.TRUE:
                        return left
                  if left == right:
                        return left
                  return BNode(B.AND, left, right)
            if b.tag == B.OR:
                  left = simplify(b.left)
                  right = simplify(b.right)
                  if left.tag == B.TRUE or right.tag == B.TRUE:
                        return BNode(B.TRUE)
                  if left.tag == B.FALSE:
                        return right
                  if right.tag == B.FALSE:
                        return left
                  if left == right:
                        return left
                  return BNode(B.OR, left, right)
      return b

class ABA:
      def __init__(self, init_state, delta):
            self.current_state = init_state
            self.delta         = delta

      def deltaP(self, state, inp):
            if state.tag == B.TRUE or state.tag == B.FALSE:
                  return state

            if state.tag == B.TERM:
                  if (state.left, inp) in self.delta:
                        return self.delta[(state.left, inp)]
                  else:
                         return BNode(B.TRUE)

            if state.tag == B.AND:
                  return BNode(B.AND, self.deltaP(simplify(state.left), inp), self.deltaP(simplify(state.right), inp))

            if state.tag == B.OR:
                  return BNode(B.OR, self.deltaP(simplify(state.left), inp), self.deltaP(simplify(state.right), inp))

      def accept_dict(self, inp=dict()):
            if type(inp) is dict:
                  s = set()

                  for k in inp.keys():
                        if inp[k]:
                              s.add(k)
                  return self.accept(s)

            return False

      def accept(self, inp=set()):
            if debug:
                print("ACCEPTING...")
            if type(inp) is set:
                  print(self.current_state)
                  s = self.deltaP(self.current_state, frozenset(inp))
                  if debug:
                        print("=========>")
                        print(s)
                        print("==simpl==>")
                  self.current_state = simplify(self.deltaP(self.current_state, frozenset(inp)))
                  if debug:
                        print(self.current_state)

                  if self.current_state.tag == B.FALSE:
                        return False

                  return True

            return False


