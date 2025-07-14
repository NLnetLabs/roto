from sphinx.directives import ObjectDescription
from sphinx.domains import Domain
from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.application import Sphinx
from sphinx.util.nodes import make_refnode

class RotoFunctionLike(ObjectDescription):
    """Generic Roto function-like object that we don't register into the domain"""
    
    class_name = ""

    def handle_signature(self, sig, signode):
        """Append the nodes for the sig into the signode"""
 
        if '.' in sig:
            receiver, _, sig = sig.partition('.')
        else:
            receiver = None

        name, _, rest = sig.partition('(')
        params, _, ret = rest.partition(')')
        
        sig_param_list = addnodes.desc_parameterlist()
        for param in params.split(','):
            if not param:
                continue
            param_name, _, ty = param.partition(': ') 
            sig_param = addnodes.desc_parameter()
            sig_param += addnodes.desc_name(text=param_name)
            sig_param += addnodes.desc_sig_punctuation(text=':')
            sig_param += addnodes.desc_sig_space(text=' ')
            sig_param += addnodes.pending_xref(
                "",
                addnodes.desc_type(text=ty),
                refdomain="roto",
                reftype="ref",
                reftarget=ty,
            )
            sig_param_list += sig_param

        signode += addnodes.desc_annotation(text=self.class_name)

        if receiver:
            signode += addnodes.pending_xref(
                "",
                addnodes.desc_addname(text=receiver),
                refdomain="roto",
                reftype="ref",
                reftarget=receiver,
            )
            signode += addnodes.desc_sig_punctuation(text='.')
    
        signode += addnodes.desc_name(text=name)
        signode += addnodes.desc_sig_punctuation('(')
        signode += sig_param_list
        signode += addnodes.desc_sig_punctuation(')')
        
        ret = ret.strip().removeprefix('->').strip()
        sig_ret = addnodes.desc_returns()
        sig_ret += addnodes.pending_xref(
            "",
            addnodes.desc_type(text=ret),
            refdomain="roto",
            reftype="ref",
            reftarget=ret,
        )

        signode += sig_ret

        signode['path'] = sig
        signode['fullname'] = fullname = f"{receiver}.{name}"
    
        return fullname

    def needs_arglist(self):
        return False

    def add_self(self, signature):
        raise NotImplemented

    def add_target_and_index(self, name_cls, sig, signode):
        signode['ids'].append('roto' + '-' + name_cls)
        roto_domain = self.env.get_domain('roto')
        roto_domain.add_obj(self.class_name.replace(' ', '_') + 's', name_cls)


class RotoType(ObjectDescription):
    def handle_signature(self, sig: str, signode):
        signode += addnodes.desc_annotation(text="type")
        signode += addnodes.desc_name(text=sig)
        return sig
    
    def _toc_entry_name(self, signode):
        text = signode.children[1].children[0]
        return str(text)
    
    def add_target_and_index(self, name_cls, sig, signode):
        signode['ids'].append('roto' + '-' + sig)

        roto_domain = self.env.get_domain('roto')
        roto_domain.add_obj('types', sig)

class RotoContext(ObjectDescription):
    def handle_signature(self, sig: str, signode):
        signode += addnodes.desc_annotation(text="context")
        name, ty = sig.split(":")
        signode += addnodes.desc_name(text=name.strip())
        signode += addnodes.desc_sig_punctuation(text=":")
        signode += addnodes.desc_sig_space(text=' ')

        ty = ty.strip()
        signode += addnodes.pending_xref(
            "",
            addnodes.desc_type(text=ty),
            refdomain="roto",
            reftype="ref",
            reftarget=ty,
        )

class RotoConstant(ObjectDescription):
    def handle_signature(self, sig: str, signode):
        signode += addnodes.desc_annotation(text="constant")
        name, ty = sig.split(":")
        signode += addnodes.desc_name(text=name.strip())
        signode += addnodes.desc_sig_punctuation(text=":")
        signode += addnodes.desc_sig_space(text=' ')

        ty = ty.strip()
        signode += addnodes.pending_xref(
            "",
            addnodes.desc_type(text=ty),
            refdomain="roto",
            reftype="ref",
            reftarget=ty,
        )

class RotoFunction(RotoFunctionLike):
    class_name = "function"

class RotoStaticMethod(RotoFunctionLike):
    class_name = "static method"

class RotoMethod(RotoFunctionLike):
    class_name = "method"

class RecipeDomain(Domain):
    name = 'roto'
    label = 'Roto'
    roles = {
        'ref': XRefRole(),
    }

    directives = {
        'function': RotoFunction,
        'method': RotoMethod,
        'static_method': RotoStaticMethod,
        'type': RotoType,
        'constant': RotoConstant,
        'context': RotoContext,
    }

    indices = []

    initial_data = {
        'functions': {},
        'methods': {},
        'static_methods': {},
        'types': {},
    }

    data_version = 0

    def all_objects(self):
        # print(self.data)
        for k, v in self.data.items():
            if k == "version":
                continue
            yield from v.values()

    def add_obj(self, category, signature):
        name = f'roto.{signature}'
        anchor = f'roto-{signature}'

        self.data[category][signature] = (
            name,
            signature,
            'Roto',
            self.env.docname,
            anchor,
            0,
        )

    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        match = [
            (docname, anchor)
            for name, sig, typ, docname, anchor, prio in self.all_objects()
            if sig == target
        ]

        if len(match) > 0:
            todocname = match[0][0]
            targ = match[0][1]
            return make_refnode(builder, fromdocname, todocname, targ, contnode, targ)
        else:
            return None

    def get_full_qualified_name(self, node):
        return f'roto.{node.arguments[0]}'


def setup(app: Sphinx):
    app.add_domain(RecipeDomain)

    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
