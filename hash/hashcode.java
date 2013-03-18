import tester.*;
import java.util.*;

class Posn {
    Integer x , y;

    Posn(Integer x, Integer y) { this.x = x; this.y = y; }

    public boolean equals(Object o) {
  if (o instanceof Posn) {	    
	    Posn p = (Posn)o;
	    return p.x == this.x && p.y == this.y;
	} else
	    return false;
	
    }

    public int hashCode() {
	return (this.x + this.y) * (1 + this.x + this.y) + this.y;
    }

}


class Examples {
    void test_equals(Tester t) {
	t.checkExpect(new Posn(1,1), new Posn(1,1));
	t.checkExpect(new Posn(1,1).equals(new Posn(1,1)), true);
	t.checkExpect(new Posn(1,1).equals(new Posn(2,1)), false);
	t.checkExpect(new Posn(1,1).equals(new Integer(7)), false);
    }
    
    void test_map(Tester t) {
	Map<Posn, String> m = new HashMap<Posn,String>();
	m.put(new Posn(0,0), "dvh");
	t.checkExpect(m.get(new Posn(0,0)), "dvh");
	t.checkExpect(m.get(new Posn(17,45)), null);
    }
}
