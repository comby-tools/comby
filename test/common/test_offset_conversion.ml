open Core

open Test_helpers

module Alpha = Test_alpha
module Omega = Test_omega

let to_string format matches =
  let f = match format with
    | `Json -> Match.pp_json_lines
    | `Text -> Match.pp
  in
  Format.asprintf "%a" f (Some "file", matches)

let run source match_template format : (string * string) =
  let specification = Configuration.Specification.create ~match_template () in
  let run ~fast =
    let result =
      Pipeline.process_single_source
        (module Omega.C)
        ~fast_offset_conversion:fast
        configuration
        (String source)
        specification
      |> function
      | Matches (m, _) -> m
      | _ -> []
    in
    to_string format result
  in
  (run ~fast:false, run ~fast:true)

let%expect_test "fast_match_offset_to_line_col_conversions" =
  let source = In_channel.read_all "example/test-match-locations/mod_fortune.c" in
  let template = ":[fn.](:[1])" in
  let slow_result, fast_result = run source template `Text in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|file:8:mod_fortune_init(liModules *mods, liModule *mod)
file:9:mod_fortune_free(liModules *mods, liModule *mod)
file:19:*fortune_rand(fortune_data *fd)
file:20:g_rand_int_range(fd->rand, 0, fd->cookies->len)
file:21:g_ptr_array_index(fd->cookies, r)
file:24:fortune_header_handle(liVRequest *vr, gpointer param, gpointer *context)
file:27:UNUSED(context)
file:30:fortune_rand(fd)
file:31:li_http_header_insert(vr->response.headers, CONST_STR_LEN("X-fortune"), GSTR_LEN(cookie))
file:36:fortune_header(liServer *srv, liWorker *wrk, liPlugin* p, liValue *val, gpointer userdata)
file:37:UNUSED(srv)
file:37:UNUSED(wrk)
file:37:UNUSED(val)
file:37:UNUSED(userdata)
file:39:!li_value_is_nothing(val)
file:40:ERROR(srv, "%s", "fortnue.header doesn't take any arguments")
file:44:li_action_new_function(fortune_header_handle, NULL, NULL, p->data)
file:47:fortune_page_handle(liVRequest *vr, gpointer param, gpointer *context)
file:49:UNUSED(context)
file:59:!li_vrequest_handle_direct(vr)
file:65:fortune_rand(fd)
file:66:li_chunkqueue_append_mem(vr->direct_out, GSTR_LEN(cookie))
file:68:li_chunkqueue_append_mem(vr->direct_out, CONST_STR_LEN("no cookies in the cookie box"))
file:74:fortune_page(liServer *srv, liWorker *wrk, liPlugin* p, liValue *val, gpointer userdata)
file:75:UNUSED(srv)
file:75:UNUSED(wrk)
file:75:UNUSED(val)
file:75:UNUSED(userdata)
file:77:!li_value_is_nothing(val)
file:78:ERROR(srv, "%s", "fortnue.header doesn't take any arguments")
file:82:li_action_new_function(fortune_page_handle, NULL, NULL, p->data)
file:85:fortune_load(liServer *srv, liPlugin* p, liValue *val, gpointer userdata)
file:92:UNUSED(userdata)
file:94:li_value_get_single_argument(val)
file:96:li_value_type(val)
file:97:ERROR(srv, "fortune.load takes a string as parameter, %s given", val ? li_value_type_string(val) : "none")
file:103:!g_file_get_contents(file, &data, &len, &err)
file:104:ERROR(srv, "could not read fortune file \"%s\". reason: \"%s\" (%d)", file, err->message, err->code)
file:105:g_error_free(err)
file:111:g_string_sized_new(127)
file:115:g_ptr_array_add(fd->cookies, line)
file:116:g_string_sized_new(127)
file:124:g_string_append_c(line, *cur)
file:127:g_string_free(line, TRUE)
file:130:g_free(data)
file:132:DEBUG(srv, "loaded %u cookies from file '%s'", count, file)
file:158:plugin_fortune_free(liServer *srv, liPlugin *p)
file:160:UNUSED(srv)
file:164:g_string_free(g_ptr_array_index(fd->cookies, i), TRUE)
file:165:g_ptr_array_free(fd->cookies, TRUE)
file:167:g_rand_free(fd->rand)
file:169:g_slice_free(fortune_data, fd)
file:172:plugin_fortune_init(liServer *srv, liPlugin *p, gpointer userdata)
file:174:UNUSED(srv)
file:174:UNUSED(userdata)
file:181:g_slice_new(fortune_data)
file:183:g_rand_new()
file:184:g_ptr_array_new()
file:188:mod_fortune_init(liModules *mods, liModule *mod)
file:191:MODULE_VERSION_CHECK(mods)
file:193:li_plugin_register(srv, "mod_fortune", plugin_fortune_init, NULL)
file:201:mod_fortune_free(liModules *mods, liModule *mod)
file:203:li_plugin_free(mods->main, mod->config)
|}]

let%expect_test "fast_match_offset_to_line_col_conversions_2" =
  let source = In_channel.read_all "example/test-match-locations/physical_lua.c" in
  let template = ":[fn.](:[1])" in
  let slow_result, fast_result = run source template `Text in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|file:11:DEF_LUA_MODIFY_GSTRING(attr)
file:12:lua_physical_attr_read_##attr(liPhysical *phys, lua_State *L)
file:13:lua_pushlstring(L, phys->attr->str, phys->attr->len)
file:17:lua_physical_attr_write_##attr(liPhysical *phys, lua_State *L)
file:19:luaL_checkstring(L, 3)
file:20:lua_tolstring(L, 3, &len)
file:21:g_string_truncate(phys->attr, 0)
file:22:g_string_append_len(phys->attr, s, len)
file:26:DEF_LUA_MODIFY_GSTRING(path)
file:27:DEF_LUA_MODIFY_GSTRING(doc_root)
file:28:DEF_LUA_MODIFY_GSTRING(pathinfo)
file:32:AR(m)
file:33:AW(m)
file:34:ARW(m)
file:40:ARW(path)
file:41:ARW(doc_root)
file:42:ARW(pathinfo)
file:52:lua_physical_index(lua_State *L)
file:57:lua_gettop(L)
file:58:lua_pushstring(L, "incorrect number of arguments")
file:59:lua_error(L)
file:62:li_lua_metatable_index(L)
file:64:li_lua_get_physical(L, 1)
file:67:lua_isnumber(L, 2)
file:68:!lua_isstring(L, 2)
file:70:lua_tostring(L, 2)
file:72:strcmp(key, physical_attribs[i].key)
file:74:.read_attr(phys, L)
file:79:lua_pushstring(L, "cannot read attribute ")
file:80:lua_pushstring(L, key)
file:81:lua_pushstring(L, " in physical")
file:82:lua_concat(L, 3)
file:83:lua_error(L)
file:88:lua_physical_newindex(lua_State *L)
file:93:lua_gettop(L)
file:94:lua_pushstring(L, "incorrect number of arguments")
file:95:lua_error(L)
file:98:li_lua_get_physical(L, 1)
file:101:lua_isnumber(L, 2)
file:102:!lua_isstring(L, 2)
file:104:lua_tostring(L, 2)
file:106:strcmp(key, physical_attribs[i].key)
file:108:.write_attr(phys, L)
file:113:lua_pushstring(L, "cannot write attribute ")
file:114:lua_pushstring(L, key)
file:115:lua_pushstring(L, "in physical")
file:116:lua_concat(L, 3)
file:117:lua_error(L)
file:130:init_physical_mt(lua_State *L)
file:131:luaL_register(L, NULL, physical_mt)
file:134:li_lua_init_physical_mt(lua_State *L)
file:135:luaL_newmetatable(L, LUA_PHYSICAL)
file:136:init_physical_mt(L)
file:138:lua_pop(L, 1)
file:141:li_lua_get_physical(lua_State *L, int ndx)
file:142:!lua_isuserdata(L, ndx)
file:143:!lua_getmetatable(L, ndx)
file:144:luaL_getmetatable(L, LUA_PHYSICAL)
file:145:lua_isnil(L, -1)
file:145:lua_isnil(L, -2)
file:145:!lua_equal(L, -1, -2)
file:146:lua_pop(L, 2)
file:149:lua_pop(L, 2)
file:150:*(liPhysical**)
file:150:lua_touserdata(L, ndx)
file:153:li_lua_push_physical(lua_State *L, liPhysical *phys)
file:157:lua_pushnil(L)
file:161:lua_newuserdata(L, sizeof(liPhysical*))
file:164:luaL_newmetatable(L, LUA_PHYSICAL)
file:165:init_physical_mt(L)
file:168:lua_setmetatable(L, -2)
|}]

let%expect_test "fast_match_offset_to_line_col_conversions_2" =
  let source = "foo(bar)" in
  let template = ":[fn.](:[1])" in
  let slow_result, fast_result = run source template `Text in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|file:1:foo(bar)
|}]

let%expect_test "correct_columns" =
  let source = "hello world" in
  let template = "hello :[1]" in
  let slow_result, fast_result = run source template `Json in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|{"uri":"file","matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"1","value":"world","range":{"start":{"offset":6,"line":1,"column":7},"end":{"offset":11,"line":1,"column":12}}}],"matched":"hello world"}]}
|}];

  let source = "hello world\nhello potato\n" in
  let template = "hello :[1]" in
  let slow_result, fast_result = run source template `Json in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|{"uri":"file","matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":25,"line":3,"column":1}},"environment":[{"variable":"1","value":"world\\nhello potato\\n","range":{"start":{"offset":6,"line":1,"column":7},"end":{"offset":25,"line":3,"column":1}}}],"matched":"hello world\\nhello potato\\n"}]}
|}];

  let source = "hello world\nhello potato" in
  let template = "hello :[1]" in
  let slow_result, fast_result = run source template `Json in
  if String.(slow_result <> fast_result) then
    print_string @@ Format.sprintf "Offset conversion does not match. Expect@.%s@.got %s" slow_result fast_result
  else
    print_string fast_result;
  [%expect_exact {|{"uri":"file","matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":24,"line":2,"column":13}},"environment":[{"variable":"1","value":"world\\nhello potato","range":{"start":{"offset":6,"line":1,"column":7},"end":{"offset":24,"line":2,"column":13}}}],"matched":"hello world\\nhello potato"}]}
|}]
