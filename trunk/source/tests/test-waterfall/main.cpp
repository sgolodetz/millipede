#include <iostream>
#include <string>

#include <source/common/ipfs/construction/Waterfall.h>
using namespace mp;

class TestWE : public WaterfallEdge
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::string m_name;

	//#################### CONSTRUCTORS ####################
public:
	TestWE(const std::string& name, int weight)
	:	WaterfallEdge(weight), m_name(name)
	{}

	//#################### PRIVATE METHODS ####################
private:
	void merge_hook() const
	{
		std::cout << "Merging edge " << m_name << std::endl;
	}
};

int main()
{
	WaterfallEdge_Ptr r(new TestWE("r", INT_MAX));
	WaterfallEdge_Ptr a(new TestWE("a", 5));
	WaterfallEdge_Ptr b(new TestWE("b", 4));
	WaterfallEdge_Ptr c(new TestWE("c", 2));
	WaterfallEdge_Ptr d(new TestWE("d", 6));
	WaterfallEdge_Ptr e(new TestWE("e", 1));
	WaterfallEdge_Ptr f(new TestWE("f", 1));
	WaterfallEdge_Ptr g(new TestWE("g", 3));
	WaterfallEdge_Ptr h(new TestWE("h", 2));
	WaterfallEdge_Ptr i(new TestWE("i", 3));
	WaterfallEdge_Ptr j(new TestWE("j", 2));

	r->add_child(a);
		a->add_child(d);
	r->add_child(b);
		b->add_child(e);
			e->add_child(h);
		b->add_child(f);
			f->add_child(i);
	r->add_child(c);
		c->add_child(g);
			g->add_child(j);

	Waterfall::iterate(r);

	return 0;
}
