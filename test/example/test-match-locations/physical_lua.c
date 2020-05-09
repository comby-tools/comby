
#include <lighttpd/core_lua.h>

#include <lualib.h>
#include <lauxlib.h>

#define LUA_PHYSICAL "liPhysical*"

typedef int (*lua_Physical_Attrib)(liPhysical *phys, lua_State *L);

#define DEF_LUA_MODIFY_GSTRING(attr)                                         \
static int lua_physical_attr_read_##attr(liPhysical *phys, lua_State *L) {   \
	lua_pushlstring(L, phys->attr->str, phys->attr->len);                    \
	return 1;                                                                \
}                                                                            \
                                                                             \
static int lua_physical_attr_write_##attr(liPhysical *phys, lua_State *L) {  \
	const char *s; size_t len;                                               \
	luaL_checkstring(L, 3);                                                  \
	s = lua_tolstring(L, 3, &len);                                           \
	g_string_truncate(phys->attr, 0);                                        \
	g_string_append_len(phys->attr, s, len);                                 \
	return 0;                                                                \
}

DEF_LUA_MODIFY_GSTRING(path)
DEF_LUA_MODIFY_GSTRING(doc_root)
DEF_LUA_MODIFY_GSTRING(pathinfo)

#undef DEF_LUA_MODIFY_GSTRING

#define AR(m) { #m, lua_physical_attr_read_##m, NULL }
#define AW(m) { #m, NULL, lua_physical_attr_write_##m }
#define ARW(m) { #m, lua_physical_attr_read_##m, lua_physical_attr_write_##m }

static const struct {
	const char* key;
	lua_Physical_Attrib read_attr, write_attr;
} physical_attribs[] = {
	ARW(path),
	ARW(doc_root),
	ARW(pathinfo),

	{ NULL, NULL, NULL }
};

#undef AR
#undef AW
#undef ARW


static int lua_physical_index(lua_State *L) {
	liPhysical *phys;
	const char *key;
	int i;

	if (lua_gettop(L) != 2) {
		lua_pushstring(L, "incorrect number of arguments");
		lua_error(L);
	}

	if (li_lua_metatable_index(L)) return 1;

	phys = li_lua_get_physical(L, 1);
	if (!phys) return 0;

	if (lua_isnumber(L, 2)) return 0;
	if (!lua_isstring(L, 2)) return 0;

	key = lua_tostring(L, 2);
	for (i = 0; physical_attribs[i].key ; i++) {
		if (0 == strcmp(key, physical_attribs[i].key)) {
			if (physical_attribs[i].read_attr)
				return physical_attribs[i].read_attr(phys, L);
			break;
		}
	}

	lua_pushstring(L, "cannot read attribute ");
	lua_pushstring(L, key);
	lua_pushstring(L, " in physical");
	lua_concat(L, 3);
	lua_error(L);

	return 0;
}

static int lua_physical_newindex(lua_State *L) {
	liPhysical *phys;
	const char *key;
	int i;

	if (lua_gettop(L) != 3) {
		lua_pushstring(L, "incorrect number of arguments");
		lua_error(L);
	}

	phys = li_lua_get_physical(L, 1);
	if (!phys) return 0;

	if (lua_isnumber(L, 2)) return 0;
	if (!lua_isstring(L, 2)) return 0;

	key = lua_tostring(L, 2);
	for (i = 0; physical_attribs[i].key ; i++) {
		if (0 == strcmp(key, physical_attribs[i].key)) {
			if (physical_attribs[i].write_attr)
				return physical_attribs[i].write_attr(phys, L);
			break;
		}
	}

	lua_pushstring(L, "cannot write attribute ");
	lua_pushstring(L, key);
	lua_pushstring(L, "in physical");
	lua_concat(L, 3);
	lua_error(L);

	return 0;
}


static const luaL_Reg physical_mt[] = {
	{ "__index", lua_physical_index },
	{ "__newindex", lua_physical_newindex },

	{ NULL, NULL }
};

static void init_physical_mt(lua_State *L) {
	luaL_register(L, NULL, physical_mt);
}

void li_lua_init_physical_mt(lua_State *L) {
	if (luaL_newmetatable(L, LUA_PHYSICAL)) {
		init_physical_mt(L);
	}
	lua_pop(L, 1);
}

liPhysical* li_lua_get_physical(lua_State *L, int ndx) {
	if (!lua_isuserdata(L, ndx)) return NULL;
	if (!lua_getmetatable(L, ndx)) return NULL;
	luaL_getmetatable(L, LUA_PHYSICAL);
	if (lua_isnil(L, -1) || lua_isnil(L, -2) || !lua_equal(L, -1, -2)) {
		lua_pop(L, 2);
		return NULL;
	}
	lua_pop(L, 2);
	return *(liPhysical**) lua_touserdata(L, ndx);
}

int li_lua_push_physical(lua_State *L, liPhysical *phys) {
	liPhysical **pphys;

	if (NULL == phys) {
		lua_pushnil(L);
		return 1;
	}

	pphys = (liPhysical**) lua_newuserdata(L, sizeof(liPhysical*));
	*pphys = phys;

	if (luaL_newmetatable(L, LUA_PHYSICAL)) {
		init_physical_mt(L);
	}

	lua_setmetatable(L, -2);
	return 1;
}
