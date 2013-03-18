import tester.*;
import java.util.*;


class HT<X,Y> {

    ArrayList<Bucket<X,Y>> table;

    Integer SIZE = 64;

    HT() {
  this.table = new ArrayList<Bucket<X,Y>>(SIZE);
	this.buildList(SIZE);
    }

    void buildList(Integer i) {
	if (i == 0) 
	    return;
	else {
	    Bucket<X,Y> b = new Bucket<X,Y>();
	    this.table.add(b);
	    this.buildList(i - 1);
	}
		
    }

    Y get(X key) {
	Integer i = key.hashCode();
	Bucket<X,Y> b = this.table.get(i % SIZE);
	return b.get(key);
    }
    
    void set(X key, Y val) {
	Integer i = key.hashCode();
	Bucket<X,Y> b = this.table.get(i % SIZE);
	b.set(key, val);
    }
}

class Pair<X,Y> {
    X x; Y y;
    Pair(X x, Y y) { this.x = x; this.y = y; }
}

class Bucket<X,Y> {
    ArrayList<Pair<X,Y>> lst;
   
    Bucket() { this.lst = new ArrayList<Pair<X,Y>>(); }  
    
    Y get(X key) { 
	return this.getHelper(key, 0);
    }

    Y getHelper(X key, Integer idx) {
	if (idx == this.lst.size())
	    throw new RuntimeException("Not here");
	else {
	    Pair<X,Y> p = this.lst.get(idx);
	    if (key.equals(p.x))
		return p.y;
	    else
		return this.getHelper(key, idx+1);
	}
    }

    void set(X key, Y val) { 
	this.setHelper(key, val, 0);	
    }

    void setHelper(X key, Y val, Integer idx) {
	if (idx == this.lst.size())
	    this.lst.add(new Pair<X,Y>(key,val));
	else {
	    Pair<X,Y> p = this.lst.get(idx);
	    if (key.equals(p.x))
		p.y = val;
	    else
		this.setHelper(key, val, idx+1);
	}
    }
}


class Examples {
    void testget(Tester t) {
	HT<String,Integer> ht = new HT<String,Integer>();
	ht.set("hello", 17);
	t.checkExpect(ht.get("hello"),17);

	ht.set("hello", 12);
	t.checkExpect(ht.get("hello"),12);

	t.checkException(new RuntimeException("Not here"),
			 ht,
			 "get",
			 "goodbye");
    }
}
