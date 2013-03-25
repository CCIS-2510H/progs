import tester.*;
import java.util.*;


class HTIter<X,Y> implements Iterator<Pair<X,Y>> {
    Bucket<X,Y>[] table;
    Integer position;
    Iterator<Pair<X,Y>> i;
    
    HTIter(Bucket<X,Y>[] b) { 
  this.table = b;
	this.position = 0;
	this.i = this.table[position].iterator();
    }

    public boolean hasNext() {

	while (!this.i.hasNext() && this.position < this.table.length) {
	    this.i = this.table[this.position].iterator();
	    this.position += 1;
	}
	
      	if (this.i.hasNext()) 
	    return true;
	else 
	    return false;
    }

    public Pair<X,Y> next() {
	if (this.i.hasNext())
	    return this.i.next();
	else {
	    this.position += 1;
	    this.i = this.table[this.position].iterator();
	    return this.next();
	}
    }

    public void remove() { throw new RuntimeException("no remove"); }
    
}

class HT<X,Y> implements Iterable<Pair<X,Y>> {

    Bucket<X,Y>[] table;
    //ArrayList<Bucket<X,Y>> table;

    Integer SIZE = 64;

    HT() {
	this.table = new Bucket[SIZE];
	this.buildList(0, SIZE);
    }

    public Iterator<Pair<X,Y>> iterator() {
	return new HTIter<X,Y>(this.table);
    }

    // Effect: initialize this.table
    void buildList(Integer i, Integer sz) {
	
	for(i = 0; i < sz; i = i + 1) {
	    Bucket<X,Y> b = new Bucket<X,Y>();
	    this.table[i] = b;	    
	}


	// if (i.equals(sz))
	//     return;
	// else {
	//     Bucket<X,Y> b = new Bucket<X,Y>();
	//     this.table[i] = b;
	//     // this.table.set(i, b);
	//     this.buildList(i + 1, sz);
	// }
    }

    // void buildList(Integer i) {
    // 	if (i == 0) 
    // 	    return;
    // 	else {
    // 	    Bucket<X,Y> b = new Bucket<X,Y>();
    // 	    this.table[i - 1] = b;
    // 	    this.buildList(i - 1);
    // 	}
    //		
    // }

    Y get(X key) {
	Integer i = key.hashCode();
	Bucket<X,Y> b = this.table[i % SIZE];
	// this.table.get(i % SIZE);
	return b.get(key);
    }
    
    void set(X key, Y val) {
	Integer i = key.hashCode();
	Bucket<X,Y> b = this.table[i % SIZE];
	b.set(key, val);
    }
}

class Pair<X,Y> {
    X x; Y y;
    Pair(X x, Y y) { this.x = x; this.y = y; }
}

class Bucket<X,Y> implements Iterable<Pair<X,Y>> {
    ArrayList<Pair<X,Y>> lst;
   
    public Iterator<Pair<X,Y>> iterator() {
	return this.lst.iterator();
    }

    Bucket() { this.lst = new ArrayList<Pair<X,Y>>(); }  
    
    Y get(X key) { 

	for(Pair<X,Y> p : this.lst) {
	    if (key.equals(p.x))
		return p.y;
	}

	throw new RuntimeException("Not here");
    }

    void set(X key, Y val) { 
	for(Pair<X,Y> p : this.lst) {
	    if (key.equals(p.x)) {
		p.y = val;
		return;
	    }
	}

	this.lst.add(new Pair<X,Y>(key,val));
    }
}


class Examples {
    void testget(Tester t) {
	HT<String,Integer> ht = new HT<String,Integer>();
	ht.set("hello", 17);
	t.checkExpect(ht.get("hello"),17);

	ht.set("hello", 12);
	t.checkExpect(ht.get("hello"),12);

	HT<String,Integer> ht2 = new HT<String,Integer>();
	t.checkExpect(ht2.iterator().hasNext(), false);

	t.checkException(new RuntimeException("Not here"),
			 ht,
			 "get",
			 "goodbye");
    }
}
