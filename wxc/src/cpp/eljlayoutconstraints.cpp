#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxLayoutConstraints_left)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->left);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_top)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->top);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_right)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->right);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_bottom)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->bottom);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_width)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->width);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_height)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->height);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_centreX)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->centreX);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_centreY)(void* self)
{
	return (void*)(&((wxLayoutConstraints*)self)->centreY);
}
	
EWXWEXPORT(void*,wxLayoutConstraints_Create)()
{
	return (void*)new wxLayoutConstraints();
}

EWXWEXPORT(void,wxIndividualLayoutConstraint_Set)(void* self,int rel,wxWindowBase* otherW,int otherE,int val,int marg)
{
	((wxIndividualLayoutConstraint*)self)->Set((wxRelationship)rel,  otherW, (wxEdge)otherE, val, marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_LeftOf)(void* self,wxWindowBase* sibling,int marg)
{
	((wxIndividualLayoutConstraint*)self)->LeftOf( sibling, (wxEdge)marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_RightOf)(void* self,wxWindowBase* sibling,int marg)
{
	((wxIndividualLayoutConstraint*)self)->RightOf(sibling, (wxEdge)marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_Above)(void* self,wxWindow* sibling,int marg)
{
	((wxIndividualLayoutConstraint*)self)->Above(sibling,marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_Below)(void* self,wxWindow* sibling,int marg)
{
	((wxIndividualLayoutConstraint*)self)->Below(sibling, marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SameAs)(void* self,wxWindowBase* otherW,int edge,int marg)
{
	((wxIndividualLayoutConstraint*)self)->SameAs(otherW, (wxEdge)edge, (wxEdge)marg);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_PercentOf)(void* self,wxWindowBase* otherW,int wh,int per)
{
	((wxIndividualLayoutConstraint*)self)->PercentOf(otherW, (wxEdge)wh, per);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_Absolute)(void* self,int val)
{
	((wxIndividualLayoutConstraint*)self)->Absolute(val);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_Unconstrained)(void* self)
{
	((wxIndividualLayoutConstraint*)self)->Unconstrained();
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_AsIs)(void* self)
{
	((wxIndividualLayoutConstraint*)self)->AsIs();
}
	
EWXWEXPORT(void*,wxIndividualLayoutConstraint_GetOtherWindow)(void* self)
{
	return (void*)((wxIndividualLayoutConstraint*)self)->GetOtherWindow();
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetMyEdge)(wxIndividualLayoutConstraint* self)
{
	return (int)self->GetMyEdge();
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SetEdge)(void* self,int which)
{
	((wxIndividualLayoutConstraint*)self)->SetEdge((wxEdge)which);
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SetValue)(void* self,int v)
{
	((wxIndividualLayoutConstraint*)self)->SetValue(v);
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetMargin)(void* self)
{
	return ((wxIndividualLayoutConstraint*)self)->GetMargin();
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SetMargin)(void* self,int m)
{
	((wxIndividualLayoutConstraint*)self)->SetMargin(m);
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetValue)(void* self)
{
	return ((wxIndividualLayoutConstraint*)self)->GetValue();
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetPercent)(void* self)
{
	return ((wxIndividualLayoutConstraint*)self)->GetPercent();
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetOtherEdge)(void* self)
{
	return ((wxIndividualLayoutConstraint*)self)->GetOtherEdge();
}
	
EWXWEXPORT(bool,wxIndividualLayoutConstraint_GetDone)(wxIndividualLayoutConstraint* self)
{
	return self->GetDone();
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SetDone)(void* self,bool d)
{
	((wxIndividualLayoutConstraint*)self)->SetDone(d);
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetRelationship)(void* self)
{
	return ((wxIndividualLayoutConstraint*)self)->GetRelationship();
}
	
EWXWEXPORT(void,wxIndividualLayoutConstraint_SetRelationship)(void* self,int r)
{
	((wxIndividualLayoutConstraint*)self)->SetRelationship((wxRelationship)r);
}
	
EWXWEXPORT(bool,wxIndividualLayoutConstraint_ResetIfWin)(wxIndividualLayoutConstraint* self,wxWindowBase* otherW)
{
	return self->ResetIfWin(otherW);
}
	
EWXWEXPORT(bool,wxIndividualLayoutConstraint_SatisfyConstraint)(wxIndividualLayoutConstraint* self,void* constraints,wxWindowBase* win)
{
	return self->SatisfyConstraint((wxLayoutConstraints*)constraints, win);
}
	
EWXWEXPORT(int,wxIndividualLayoutConstraint_GetEdge)(void* self,int which,wxWindowBase* thisWin,wxWindowBase* other)
{
	return ((wxIndividualLayoutConstraint*)self)->GetEdge((wxEdge)which,  thisWin, other);
}
	
}
