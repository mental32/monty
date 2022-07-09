// use montyc_hlirt::{glue::HostGlue, typing::PythonType, Function};

// use crate::prelude::GlobalContext;

// pub fn func(gcx: &mut GlobalContext, f: &Function) -> String {
//     let module = gcx.get_module(f.mref).unwrap();
//     let debug_info = format!("{:#?}", module.data)
//         .lines()
//         .map(|line| format!("# {}\n", line))
//         .collect::<String>();

//     let func_display = {
//         let name = gcx.spanref_to_str(f.name);
//         let (params, ret) = {
//             let tcx = gcx.tcx().borrow();

//             if let PythonType::Callable { args, ret } = tcx.get_python_type_of(f.type_id).unwrap() {
//                 let args = args.unwrap_or_default();
//                 let args = args
//                     .iter()
//                     .map(|arg| tcx.display_type(arg.clone()).unwrap())
//                     .collect::<Vec<_>>();

//                 let ret = tcx.display_type(ret).unwrap();

//                 (args.join(", "), ret)
//             } else {
//                 todo!();
//             }
//         };

//         let body = {
//             let mut lines = Vec::with_capacity(f.code.inst().len());

//             for inst in &f.code {
//                 lines.push(format!("    %{} {}", inst.value, inst.op));
//             }

//             lines.join("\n")
//         };

//         format!("def {}({}) -> {} {{\n{}\n}}", name, params, ret, body)
//     };

//     format!("{}{}", debug_info, func_display)
// }
