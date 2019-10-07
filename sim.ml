[@@@warning "-23"]  (* disable warning about supposedly useless "with" clauses *)

open Util
open SimCommon
open BankAccounts
open Monads
       
open SimCommon.Sim
open SimCommon.SimEnv
open SimCommon.SimLog
open SimCommon.SimParam
                      
module Env = StateAbs(SimEnv)
module Log = StateAbs_Writer(SimLog)
module Sfc = StateAbs_Accs
module ParT = ReaderT(SimParam)
                       
module EnvLog = Log.T(Env)
module EnvLogSfc = Sfc.T(EnvLog)

(* the "big monad" of the simulation *)
(* because using lift is cumbersome *)
(* did not find a better way yet *)
module EnvLogSfcPar = struct
  include ParT(EnvLogSfc)
  let get_env = lift_ext (EnvLogSfc.lift_ext (EnvLog.lift_ext Env.get_state))
  let set_env = lift_ext (EnvLogSfc.lift_ext (EnvLog.lift_ext Env.set_state))
  let log = lift_ext (EnvLogSfc.lift_ext EnvLog.write)
  let get_accs = lift_ext (EnvLogSfc.get_accs)
  let get_amount = lift_ext (EnvLogSfc.get_amount)
  let exc_make_transfert = lift_ext (EnvLogSfc.exc_make_transfert)
  let run ma param accs env =
    Env.run (EnvLog.run (EnvLogSfc.run (ma param) accs)) env
end

open EnvLogSfcPar

let liste_fold_left_mconc l b f : 'b monad =
  with_mconc b { f = fun mcb ->
    Liste.fold_left l mcb (fun elt -> apply' (f elt)) }

let agents_fold_mconc agents b f : 'b monad =
  with_mconc b { f = fun mcb ->
      Agents.fold agents mcb (fun key elt -> apply' (f key elt)) }
        
let incrPeriodNb sim : Sim.t = { sim with periodNb = sim.periodNb + 1 }

let randInt borne : int monad = (* borne supérieure exclusive *)
  perform begin
    env <-- get_env () ;
    let (rand_int, new_rand) = Rand.genInt env.rand borne in
    set_env { env with rand = new_rand } ;
    return rand_int
  end

(* utilise des mutations internes *)
(* attention aux modifications *)
let shuffleArray arr : 'a array monad =
  let n = (Array.length arr) - 1 in
  (arr, 0)
  >@ m_for n (fun (arr, i) ->
    perform begin
      k <-- randInt (i + 1) ;
      let tmp = arr.(i) in
      let () = arr.(i) <- arr.(k) in
      let () = arr.(k) <- tmp in
      return (arr, i + 1)
    end)
  >@ map' fst
    
let shuffleList la : 'a list monad =
  let arr = Array.of_list la in
  shuffleArray arr
  >@ map' Array.to_list

(* Mélange puis découpe une liste en plusieurs morceaux. *)
(* On impose que la taille de la liste soit un multiple de *)
(* la taille d'un morceau. *)
(* Lorsque la taille de la liste est inférieure à *)
(* la taille d'un morceau fois le nombre de morceaux, *)
(* on recommence avec la liste initiale, mélangée à nouveau. *)
let shuffleAndCut la nb_parts size_part : 'a list list monad =
  let size_la = Liste.size la in
  if size_la mod size_part <> 0 then failwith "shuffleAndCut : args incorrects";
  let arrA = Array.of_list la in
  let make_parts nb_parts la : (int * 'a list list) monad =
    (nb_parts, la, [])
    >@ m_while4 (fun (nb_parts, la, lpa) ->
      if nb_parts <= 0
      then return None
      else return (match Liste.take size_part la with
      | Some (tla,pa) -> Some (nb_parts - 1, tla, (pa :: lpa))
      | None -> None) )
    >@ map' (fun (nb_parts, la, lpa) -> (nb_parts, lpa))
  in
  (nb_parts, [])
  >@ m_while
    (fun (nb_parts, lpa) -> nb_parts > 0)
    (fun (nb_parts, lpa) ->
      perform begin
        shuffleArray arrA ; (* mutates arrA *)
        let la = Array.to_list arrA in
        (nb_parts2, lpa2) <-- make_parts nb_parts la ;
        return (nb_parts2, Liste.append lpa2 lpa)
      end)
  >@ map' snd

let new_ident () : ident monad =
  perform begin
    env <-- get_env () ;
    set_env { env with nextIdent = env.nextIdent + 1 } ;
    return env.nextIdent
  end

let new_menage () : (menage * ident) monad =
  perform begin
    id <-- new_ident () ;
    let menage = { id ;
                   nom = "menage-" ^ i2s id ;
                   biens = 0 }
    in
    log (Fifo.one (CreationMenage id)) ;
    return (menage, id)
  end
    
let new_firm () : (firm * ident) monad =
  perform begin
    param <-- get_param () ;
    id <-- new_ident () ;
    let firm = { id ;
                 nom = "firm-" ^ i2s id ;
                 prix = param.prix ;
                 machines = (
                   Liste.make
                     param.nbMachinesParFirm
                     (fun _ -> { etape = 0 }) ) ;
                 biens = 0 }
    in
    log (Fifo.one (CreationFirm id)) ;
    return (firm, id)
  end

let new_agents (new_agent : unit -> ('a * ident) monad) n
    : 'a Agents.t monad =
  Agents.nil
  >@ m_for n (fun agents ->
           perform begin
                 (agent, id) <-- new_agent () ;
                 return (Agents.set id agent agents)
              end)

let incr_periodNb sim = { sim with periodNb = sim.periodNb + 1 }

let revenu_univ_1 id_menage sim : unit monad =
  perform begin
    param <-- get_param () ;
    exc_make_transfert (sim.bank.capitalAccId, param.salaire, id_menage) ;
    log (Fifo.one (RevenuUniversel (id_menage, param.salaire))) ;
    return ()
  end

let revenu_univ_all sim : unit monad =
  agents_fold_mconc sim.menages ()
     (fun id_menage agent () -> revenu_univ_1 id_menage sim)
           
let travail_machine_1 machine : (machine * bool) monad =
  perform begin
    param <-- get_param () ;
    if (machine.etape + 1) < param.etapeMax
    then return ({ machine with etape = machine.etape + 1 }, false)
    else return ({ machine with etape = 0 }, true)
  end

let travail_machine_firm firm : firm monad =    
  perform begin
    param <-- get_param () ;
    (machines, biens) <--
       liste_fold_left_mconc firm.machines ([], 0)
          (fun machine (machines, biens) ->
             perform begin
                   (machine2, fini) <-- travail_machine_1 machine ;
                   if fini
                   then return (machine2 :: machines, biens + param.nbBiensParMachine)
                   else return (machine2 :: machines, biens)
                end) ; 
    log (Fifo.one (TravailMachines (firm.id, Liste.size machines, biens))) ;
    return { firm with machines ; biens = firm.biens + biens }
  end

let production sim : Sim.t monad =
  perform begin
        firms <--
           agents_fold_mconc sim.firms sim.firms
              (fun id firm firms ->
                 perform begin
                       firm2 <-- travail_machine_firm firm ;
                       return (Agents.set id firm2 firms)
                    end) ;
    return { sim with firms }
  end
    
let achete_1 (menage : menage) (firm : firm) : (menage * firm) monad =
  perform begin
    param <-- get_param () ;
    amount <-- get_amount menage.id ;
    let nb_achats = min (amount / param.prix) firm.biens in
    let cout = nb_achats * firm.prix in
    if nb_achats = 0
    then return ()
    else
      perform begin
        exc_make_transfert (menage.id, cout, firm.id) ;
        log (Fifo.one (
          Achat (menage.id, firm.id, nb_achats, firm.prix) ))
      end ;
    let menage = { menage with biens = menage.biens + nb_achats } in
    let firm = { firm with biens = firm.biens - nb_achats } in
    return (menage, firm)
  end

let achete_all menage firms : (menage * firm list) monad =
  liste_fold_left_mconc firms (menage, [])
     (fun firm (menage, firms) ->
        perform begin
              (menage2, firm2) <-- achete_1 menage firm ;
              return (menage2, firm2 :: firms)
           end)


let distribution sim : Sim.t monad =
  perform begin
    param <-- get_param () ;
    (rand_menages : menage list) <-- shuffleList (Agents.values sim.menages) ;
    (rand_firms_id_part : ident list list) <-- (
      let firmsId = Agents.keys sim.firms in
      let nbMenages = Agents.size sim.menages in
      let nbFirmsForeach = param.menageVision in
      shuffleAndCut firmsId nbMenages nbFirmsForeach ) ;
    liste_fold_left_mconc (Liste.zip rand_menages rand_firms_id_part) sim
       (fun (menage, firms_id_part) sim ->
          perform begin
                let firms_part =
                  Liste.fmap (fun id -> Agents.exc_get id sim.firms) firms_id_part
                in
                (menage2, firms_part2) <-- achete_all menage firms_part ;
                let menages2 = Agents.set menage.id menage2 sim.menages in
                let firms2 =
                  Liste.fold_left firms_part2 sim.firms
                     (fun firm2 firms -> Agents.set firm2.id firm2 firms)
                     
                in
                return { sim with menages = menages2 ; firms = firms2 }
             end)       
     end

let m_sim_init : Sim.t monad =
  perform begin
    param <-- get_param () ;
    menages <-- new_agents new_menage param.nb_menages ;
    firms <-- new_agents new_firm param.nb_firms ;
    return { Sim.init with menages ; firms }
  end

let param : SimParam.t = {
  nb_menages = 1_000 ;
  nb_firms = 100 ;
  prix = 100 ;
  salaire = 1_000 ;
  etapeMax = 5 ;
  nbMachinesParFirm = 10 ;
  nbBiensParMachine = 50 ;
  menageVision = 5
}
    
let period sim : Sim.t monad =
  perform begin
    let sim = incrPeriodNb sim in
    revenu_univ_all sim ;
    sim <-- production sim ;
    sim <-- distribution sim ;
    return sim 
  end

let main =
  let n = Sys.argv.(1) >@ s2i in
  
  let rec loop : 'id. (Sim.t,'id) mconc -> (Sim.t,'id) mconc =
    fun mcsim ->
    let ((((sim, param), accs), logs), env) = extract mcsim in
    (*let () = print_endline (sim2s sim env logs accs) in*)
    if sim.periodNb < n
    then loop (mcsim >>=: period)
    else mcsim
  in
  
  let m_sim : Sim.t monad = m_sim_init >>= with_mconc' { f = loop } in

  run m_sim param Accs.nil SimEnv.init
