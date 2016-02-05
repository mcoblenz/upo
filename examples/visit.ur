(* Adapted version of app for MIT CS admitted PhD student visit weekend *)

open Bootstrap3
structure Theme = Ui.Make(Default)

(* Has the visit weekend started yet?  (Only admins may set it.) *)
table visitStarted : { Started : bool }

(* Has meeting scheduling started yet?  (Only admins may set it.) *)
table schedulingStarted : { Started : bool }

(* Local CSAIL people *)
table local : { CsailId : string, LocalName : string, IsAdmin : bool, IsPI : bool, Attending : bool,
                FiveMinute : string, PhoneNumber : string, DietaryRestriction : string, Office : string,
                Transport : string, Password : string }
  PRIMARY KEY CsailId,
  CONSTRAINT LocalName UNIQUE LocalName

val localShow : show {LocalName : string} = mkShow (fn r => r.LocalName)
val csailIdShow : show {CsailId : string} = mkShow (fn r => r.CsailId)
val officeShow : show {Office : string} = mkShow (fn r => " (" ^ r.Office ^ ")")
val localNameRead : read {LocalName : string} = mkRead' (fn s => Some {LocalName = s}) "localName"
val csailIdRead : read {CsailId : string} = mkRead' (fn s => Some {CsailId = s}) "CS username"

(* Admitted students *)
table admit : { AdmitId : int, AdmitName : string, Attending : bool,
                PhoneNumber : string, Email : string, DietaryRestriction : string,
                LodgingPref : string, Arrival : string, Departure : string,
                Gender : string, Affiliation : string }
  PRIMARY KEY AdmitName,
  CONSTRAINT AdmitId UNIQUE AdmitId

val admitShow : show {AdmitName : string} = mkShow (fn r => r.AdmitName)
val admitRead : read {AdmitName : string} = mkRead' (fn s => Some {AdmitName = s}) "admitName"

(* Legal meeting times *)
table time : { Time : time}
  PRIMARY KEY (Time)

val timeShow : show {Time: time} =
    mkShow (fn r => show r.Time)
val timeRead : read {Time: time} =
    mkRead' (fn s => 
    		case (read s) of 
    			(Some s') => Some {Time = s'}
    			    	| _ => None) "time"
    

(* Scheduled dinners for specific research areas *)
table dinner : { Dinner : string, Description : string }
  PRIMARY KEY Dinner

val dinnerShow : show {Dinner : string} = mkShow (fn r => r.Dinner)

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM local);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO local(CsailId, LocalName, IsAdmin, IsPI, Attending, Office, PhoneNumber, DietaryRestriction, FiveMinute, Transport, Password)
           VALUES ('admin', 'Admin', TRUE, TRUE, FALSE, '', '', '', '', '', 'SIGBOVIK'))

(* The real app uses client certificates, but here we'll do cookies for convenience. *)
cookie localC : string
cookie localPassword : string



(* Check credentials as admin. *)
val authAdmin (username : string, password : string) =
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM local
              WHERE local.CsailId = {[username]} 
                AND local.IsAdmin 
                AND local.Password = {[password]})

(* Check credentials as a generic local. *)
val authLocal (username : string, password : string) =
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM local
              WHERE local.CsailId = {[username]}
                AND local.Password = {[password]})

(* Return True iff we're logged in as an admin. *)
val amAdmin = username <- getCookie localC;
    	      password <- getCookie localPassword;
	      case (username, password) of
	      	   (Some u, Some p) => authAdmin(u, p)
		  | _ => return False 

val amLocal = username <- getCookie localC;
    	      password <- getCookie localPassword;
	      case (username, password) of
	      	   (Some u, Some p) => authLocal(u, p)
		  | _ => return False 

val amPi = username <- getCookie localC;
           password <- getCookie localPassword;
	   case (username, password) of
                   (Some u, Some p) =>
		   	 oneRowE1 (SELECT COUNT( * ) > 0
			               FROM local
			          WHERE local.CsailId = {[u]}
			      	    AND local.IsPI
		                    AND local.Password = {[p]})
	          | _ => return False

val requireAdmin =
    isAdmin <- amAdmin;
    if isAdmin then
        return ()
    else
        error <xml>Access denied to admin section</xml>

val requireLocal =
    isLocal <- amLocal;
    if isLocal then
        return ()
    else
        error <xml>Access denied to local section</xml>

val usernameLocal = 
    isLocal <- amLocal;
    lo <- getCookie localC;

    case lo of
    	  None => error <xml>Please report this bug.</xml>
        | Some r => return r

(* Surprise twist: masquerade mode, for admin to pretend to be another PI! *)
cookie masquerade : string (* LocalName *)

val usernamePiOrAdmin =
    requireLocal;
    user <- getCookie localC;
    isAdmin <- amAdmin;

    if isAdmin then
        ma <- getCookie masquerade;
        (case ma of
             None => return user
           | Some _ => return ma)
    else
        return user

val requirePi =
    isPi <- amPi;
    if isPi then return (getCookie localC)
    else  error <xml>Access denied</xml>

val requirePiOrAdmin =
    isPi <- usernamePiOrAdmin;
    case isPi of
        None => error <xml>Access denied</xml>
      | Some name => return name

(* Authenticating admits by their assigned random IDs *)
cookie admitC : int

val admitId =
    id <- getCookie admitC;
    case id of
        None => return None
      | Some id =>
        accurate <- oneOrNoRowsE1 (SELECT (admit.AdmitName)
                                   FROM admit
                                   WHERE admit.AdmitId = {[id]});
        case accurate of
            None => return None
          | Some name => return (Some (id, name))

val requireAdmit =
    o <- admitId;
    case o of
        None => error <xml>Not properly authenticated as an admitted student</xml>
      | Some r => return r

val amHome = user <- usernameLocal; return (Some {CsailId = user})
val amHomeOrAdmin =
    user <- usernameLocal;
    ma <- getCookie masquerade;
    case ma of
        None => return (Some {CsailId = user})
      | Some ma =>
        isAdmin <- oneRowE1 (SELECT COUNT( * ) > 0
                             FROM local
                             WHERE local.CsailId = {[user]}
                               AND local.IsAdmin);
        if isAdmin then
            return (Some {CsailId = ma})
        else
            error <xml>Not authorized to masquerade</xml>
val amPi = user <- requirePi; return (Some {CsailId = user})
val amPiOrAdmin = user <- requirePiOrAdmin; return (Some {CsailId = user})
val amAway = o <- admitId;
    return (case o of
                None => None
              | Some (_, name) => Some {AdmitName = name})

structure Dinners = Rsvp2.Make(struct
                                   val homeLabel = "Faculty & Students"
                                   val awayLabel = "Prospectives"

                                   con homeKey = [CsailId = _]
                                   con awayKey = [AdmitName = _]

                                   val home = local
                                   val away = admit
                                   val event = dinner

                                   val homeDataLabels = {DietaryRestriction = "Dietary Restriction", Transport = "Transportation", LocalName = "Name"}
                                   val homeSensitiveDataLabels = {PhoneNumber = "Phone Number"}
                                   val awayDataLabels = {DietaryRestriction = "Dietary Restriction"}
                                   val awaySensitiveDataLabels = {PhoneNumber = "Phone Number", Email = "E-mail Address"}
                                   fun render {Description = d} = txt d

                                   val amHome = amHomeOrAdmin
                                   val amPrivilegedHome = amPiOrAdmin
                                   val amAway = amAway
                               end)

structure Meetings = MeetingGrid.Make(struct
	   	                          con homeKey = [CsailId = _]
                                          con homeOffice = [Office = _]
                                          con awayKey = [AdmitName = _]
                                          con awayFilter = []

                                          val home = local
                                          val away = admit
                                          val time = time

                                          val homeHardConst = {IsPI = True}
                                          val homeSoftConst = {Attending = True}
                                          val awayConst = {Attending = True}

                                          val amHome = amPiOrAdmin
                                          val amAway =
                                              (* Here's a little hack.
                                               * PIs should be allowed to authenticate as
                                               * a dummy admit, to view the by-admit grid. *)
                                              opt <- amAway;
                                              case opt of
                                                  Some _ => return opt
                                                | _ =>
                                                  opt <- amPi;
                                                  case opt of
                                                      None => return None
                                                    | Some _ => return (Some {AdmitName = ""})
                                      end)

val explainMeetings = Ui.h4 <xml>Each meeting slot is 30 minutes, starting at the listed time.</xml>

(* Pages intended for admits *)
structure Admits = struct
    structure InputAdmit = InputStrings.Make(struct
                                                 val const = {}
                                                 con given = [AdmitName = _]
                                                 con fixed = [Attending = _, AdmitId = _]
                                                 val tab = admit
                                                 val chosenLabels = {PhoneNumber = "Mobile Phone#",
                                                                     Email = "E-mail",
                                                                     DietaryRestriction = "Dietary Restriction",
                                                                     LodgingPref = "Lodging preference (hotel, with current student, other...)",
                                                                     Arrival = "Arrival date & time",
                                                                     Departure = "Departure date & time",
                                                                     Gender = "Gender",
                                                                     Affiliation = "Current school or company (optional; might appear on name tag)" }
                                                 val textLabel = "Personal information"
                                                 val amGiven = amAway
                                             end)

    val rsvp =
        (id, _) <- requireAdmit;
        dml (UPDATE admit
             SET Attending = TRUE
             WHERE AdmitId = {[id]})

    val unrsvp =
        (id, name) <- requireAdmit;
        dml (UPDATE admit
             SET Attending = FALSE
             WHERE AdmitId = {[id]});
        Meetings.Away.deleteFor {AdmitName = name}

    fun main id =
        setCookie admitC {Value = id,
                          Secure = False,
                          Expires = None};
        (id, name) <- requireAdmit;
        key <- return {AdmitName = name};

        mode <- oneRowE1 (SELECT COUNT( * ) > 0
                          FROM visitStarted);

        attending <- oneRowE1 (SELECT (admit.Attending)
                               FROM admit
                               WHERE admit.AdmitId = {[id]});
        attending <- source attending;

        Theme.simple ("CMU CS Admitted PhD Student Portal (" ^ name ^ ")")
                  (Ui.seq
                       (Ui.const <xml>
                         <dyn signal={att <- signal attending;
                                      return (if att then <xml>
                                        <h2>Current RSVP: <b>YES, attending the weekend</b></h2>
                                        <button class="btn btn-primary"
                                                onclick={fn _ =>
                                                            rpc unrsvp;
                                                            set attending False}>
                                          Change RSVP to <b>NO</b>
                                        </button>
                                      </xml> else <xml>
                                        <h2>Current RSVP: <b>NO, not attending the weekend</b></h2>
                                        <button class="btn btn-primary"
                                                onclick={fn _ =>
                                                            rpc rsvp;
                                                            set attending True}>
                                          Change RSVP to <b>YES</b>
                                        </button>
                                      </xml>)}/>
                       </xml>,

                       InputAdmit.ui key,

                       Ui.moded mode
                          (Ui.seq
                               (Ui.h2 <xml>Your meeting schedule</xml>,
                                explainMeetings,
                                Meetings.Away.One.ui key))
                          (Ui.seq
                               (Ui.h2 <xml>People you would like to meet with</xml>,
                                Meetings.Away.Prefs.ui key,
                                Ui.h2 <xml>Times when you are unavailable for 1-on-1 meetings</xml>,
                                explainMeetings,
                                Meetings.Away.Unavail.ui key)),

                        Ui.h2 <xml>Meals</xml>,

                        Dinners.Away.ui key))
end

(* Pages intended for CSAILers *)
structure Locals = struct
    structure EditLocal = EditGrid.Make(struct
                                            con key = [CsailId = _]
                                            val tab = local
                                            val labels = {CsailId = "CS username",
                                                          LocalName = "Name",
                                                          IsAdmin = "Admin?",
                                                          IsPI = "PI?",
                                                          Attending = "Attending?",
                                                          FiveMinute = "Madness?",
                                                          Office = "Office",
                                                          PhoneNumber = "Phone#",
                                                          DietaryRestriction = "Dietary Restriction",
                                                          Transport = "Transport",
                                                          Password = "Password"}
                                            val authorized = amAdmin
                                        end)

    structure EditAdmit = EditGrid.Make(struct
                                            con key = [AdmitName = _]
                                            val tab = admit
                                            val labels = {AdmitId = "ID#",
                                                          AdmitName = "Name",
                                                          Attending = "Attending?",
                                                          PhoneNumber = "Phone#",
                                                          Email = "E-mail",
                                                          DietaryRestriction = "Dietary Restriction",
                                                          LodgingPref = "Lodging Preference",
                                                          Arrival = "Arrival",
                                                          Departure = "Departure",
                                                          Gender = "Gender",
                                                          Affiliation = "Affiliation"}
                                            val authorized = amAdmin
                                        end)

    structure EditTime = EditGrid.Make(struct
                                           con rest = []
                                           val tab = time
                                           val labels = {Time = "Date/Time"}
                                           val authorized = amAdmin
                                       end)

    structure EditDinner = EditGrid.Make(struct
                                             con key = [Dinner = _]
                                             val tab = dinner
                                             val labels = {Dinner = "Name",
                                                           Description = "Description"}
                                             val authorized = amAdmin
                                         end)

    structure InputPi = InputStrings.Make(struct
                                              val const = {}
                                              con given = [CsailId = _]
                                              con fixed = [LocalName = _, IsAdmin = _, IsPI = _, Attending = _, Password = _, FiveMinute = _]
                                              val tab = local
                                              val chosenLabels = {PhoneNumber = "Mobile Phone#",
                                                                  DietaryRestriction = "Dietary Restriction",
                                                                  Office = "Office",
                                                                  Transport = "Transportation to dinner, if going (driving, shuttle with admits [if your dinner has a shuttle], ...)"}
                                              val textLabel = "Personal information"
                                              val amGiven = amHomeOrAdmin
                                          end)

    val rsvp =
        name <- requirePiOrAdmin;
        dml (UPDATE local
             SET Attending = TRUE
             WHERE CsailId = {[name]})

    val unrsvp =
        name <- requirePiOrAdmin;
        dml (UPDATE local
             SET Attending = FALSE
             WHERE CsailId = {[name]});
        Meetings.Home.deleteFor {CsailId = name}

    structure Lodging = SimpleQuery.Make(struct
                                             con fs = [AdmitName = _, Email = _,
                                                       LodgingPref = _, Arrival = _,
                                                       Departure = _, Gender = _]
                                             val query = (SELECT admit.AdmitName AS AdmitName,
                                                            admit.Email AS Email,
                                                            admit.LodgingPref AS LodgingPref,
                                                            admit.Arrival AS Arrival,
                                                            admit.Departure AS Departure,
                                                            admit.Gender AS Gender
                                                          FROM admit
                                                          WHERE admit.Attending
                                                          ORDER BY admit.AdmitName DESC)
                                             val labels = {AdmitName = "Name",
                                                           Email = "E-mail",
                                                           LodgingPref = "Lodging?",
                                                           Arrival = "Arrival",
                                                           Departure = "Departure",
                                                           Gender = "Gender"}
                                         end)

    structure Rsvp = SimpleQuery.Make(struct
                                          con fs = [AdmitName = _, Email = _]
                                          val query = (SELECT admit.AdmitName AS AdmitName,
                                                         admit.Email AS Email
                                                       FROM admit
                                                       WHERE admit.Attending
                                                       ORDER BY admit.AdmitName DESC)
                                          val labels = {AdmitName = "Name",
                                                        Email = "E-mail"}
                                      end)

    structure NoRsvp = SimpleQuery.Make(struct
                                            con fs = [AdmitName = _, Email = _]
                                            val query = (SELECT admit.AdmitName AS AdmitName,
                                                           admit.Email AS Email
                                                         FROM admit
                                                         WHERE NOT admit.Attending
                                                         ORDER BY admit.AdmitName DESC)
                                             val labels = {AdmitName = "Name",
                                                           Email = "E-mail"}
                                         end)

    structure PiRsvp = SimpleQuery.Make(struct
                                            con fs = [LocalName = _, CsailId = _]
                                            val query = (SELECT local.LocalName AS LocalName,
                                                           local.CsailId AS CsailId
                                                         FROM local
                                                         WHERE local.IsPI AND local.Attending
                                                         ORDER BY local.LocalName DESC)
                                            val labels = {LocalName = "Name",
                                                          CsailId = "CS username"}
                                        end)

    structure PiNoRsvp = SimpleQuery.Make(struct
                                              con fs = [LocalName = _, CsailId = _]
                                              val query = (SELECT local.LocalName AS LocalName,
                                                             local.CsailId AS CsailId
                                                           FROM local
                                                           WHERE local.IsPI AND NOT local.Attending
                                                           ORDER BY local.LocalName DESC)
                                             val labels = {LocalName = "Name",
                                                           CsailId = "CS username"}
                                          end)

    (* PI portal *)
    fun pi masqAs =
        (case masqAs of
             "" =>
             clearCookie masquerade
           | _ =>
             Monad.ignore requireAdmin;
             setCookie masquerade {Value = masqAs,
                                   Expires = None,
                                   Secure = False});

        user <- requirePiOrAdmin;
        key <- return {CsailId = user};

        (attending, fivemin) <- oneRow (SELECT (local.Attending), (local.FiveMinute)
                                        FROM local
                                        WHERE local.CsailId = {[user]});
        attending <- source attending;
        fivemin <- source fivemin;

        siteMode <- oneRowE1 (SELECT COUNT( * ) > 0
                              FROM visitStarted);

        schMode <- oneRowE1 (SELECT COUNT( * ) > 0
                             FROM schedulingStarted);

        Theme.tabbed "CSD Open House Portal"
                  ((Some "Profile",
                    Ui.seq (Ui.const <xml>
                      <dyn signal={att <- signal attending;
                                   return (if att then <xml>
                                     <h2>Current RSVP: <b>YES, can meet with students</b></h2>
                                     <button class="btn btn-primary"
                                              onclick={fn _ =>
                                                          rpc unrsvp;
                                                          set attending False}>
                                       Change RSVP to <b>NO</b>
                                     </button>
                                   </xml> else <xml>
                                     <h2>Current RSVP: <b>NO, can't meet with students</b></h2>
                                     <button class="btn btn-primary"
                                             onclick={fn _ =>
                                                         rpc rsvp;
                                                         set attending True}>
                                       Change RSVP to <b>YES</b>
                                     </button>
                                   </xml>)}/>
                       </xml>,
                    InputPi.ui key)),
                   (if not schMode then Some "Meeting Preferences" else None,
                    Ui.seq
                        (Ui.const <xml>
                          <dyn signal={att <- signal attending;
                                       return (if att then
                                                   <xml/>
                                               else
                                                   <xml><h2>You're currently marked as <i>not attending</i>, so presumably there's no need for you to fill anything out here!  If you've changed your mind, please click the button on the "Profile" page.</h2></xml>)}/>
                         </xml>,
                         Ui.h2 <xml>Admitted students you want to meet with</xml>,
                         Meetings.Home.Prefs.ui key,
                         Ui.hr,
                         Ui.h2 <xml>Times when you are unavailable</xml>,
                         explainMeetings,
                         Meetings.Home.Unavail.ui key)),
                   (if schMode then Some "Your Meetings" else None,
                    Ui.seq (explainMeetings, Meetings.Home.One.ui key)),
                   (if schMode then Some "Meetings by PI" else None,
                    Meetings.Home.FullGrid.ui),
                   (if schMode then Some "Meetings by Admit" else None,
                    Meetings.Away.FullGrid.ui (fn _ => return True)),
                   (Some "Meals RSVP",
                    Ui.seq
                        (Ui.h3 <xml>Please RSVP for at most one lunch on March 14!</xml>,
                         Dinners.HomePrivileged.ui key)),
                   (Some "Prospectives (RSVPd)",
                    Rsvp.ui),
                   (Some "Prospectives (No RSVP)",
                    NoRsvp.ui),
                   (Some "Faculty/students (RSVPd)",
                    PiRsvp.ui),
                   (Some "Faculty/students (No RSVP)",
                    PiNoRsvp.ui))

    fun importPIs s =
        requireAdmin;
        List.app (fn r =>
                      
                     alreadyUsing <- oneRowE1 (SELECT COUNT( * ) > 0
                                               FROM local
                                               WHERE local.CsailId = {[r.CsailId]}
                                                 AND (local.IsAdmin OR local.Attending));
                     if alreadyUsing then
                         return ()
                     else
                         newPassword <- rand;
                         Sql.easy_insertOrUpdate [[CsailId = _]]
                                                 local (r ++ {IsAdmin = False, IsPI = True, Attending = False,
                                                              PhoneNumber = "", DietaryRestriction = "", FiveMinute = "",
                                                              Transport = "", Password = show newPassword}))
                 (Csv.parse s)

    fun importAdmits s =
        requireAdmin;
        List.app (fn r =>
                     id <- rand;
                     Sql.easy_insertOrSkip [[AdmitName = _]]
                                           admit ({AdmitId = id} ++ r ++ {Attending = False,
                                                                          PhoneNumber = "",
                                                                          DietaryRestriction = "",
                                                                          LodgingPref = "",
                                                                          Arrival = "",
                                                                          Departure = "",
                                                                          Gender = "",
                                                                          Affiliation = ""}))
                 (Csv.parse s)

    val startVisit =
        requireAdmin;
        dml (DELETE FROM visitStarted
             WHERE TRUE)

    val suspendVisit =
        requireAdmin;
        dml (INSERT INTO visitStarted(Started)
             VALUES (TRUE))

    val startScheduling =
        requireAdmin;
        dml (DELETE FROM schedulingStarted
             WHERE TRUE)

    val suspendScheduling =
        requireAdmin;
        dml (INSERT INTO schedulingStarted(Started)
             VALUES (TRUE))

    val scheduleMore =
        requireAdmin;
        Meetings.scheduleSome

    structure Madness = SimpleQuery.Make(struct
                                             con fs = [LocalName = _, Madness = _]
                                             val query = (SELECT local.LocalName AS LocalName,
                                                            local.FiveMinute AS Madness
                                                          FROM local
                                                          WHERE local.FiveMinute <> ''
                                                          ORDER BY local.LocalName DESC)
                                             val labels = {LocalName = "PI",
                                                           Madness = "Talk Title"}
                                         end)

    structure Lodging = SimpleQuery.Make(struct
                                             con fs = [AdmitName = _, Email = _,
                                                       LodgingPref = _, Arrival = _,
                                                       Departure = _, Gender = _]
                                             val query = (SELECT admit.AdmitName AS AdmitName,
                                                            admit.Email AS Email,
                                                            admit.LodgingPref AS LodgingPref,
                                                            admit.Arrival AS Arrival,
                                                            admit.Departure AS Departure,
                                                            admit.Gender AS Gender
                                                          FROM admit
                                                          WHERE admit.Attending
                                                          ORDER BY admit.AdmitName DESC)
                                             val labels = {AdmitName = "Name",
                                                           Email = "E-mail",
                                                           LodgingPref = "Lodging?",
                                                           Arrival = "Arrival",
                                                           Departure = "Departure",
                                                           Gender = "Gender"}
                                         end)

    structure Dietary = SimpleQuery.Make(struct
                                             val query = (SELECT local.DietaryRestriction AS Diet
                                                          FROM local
                                                          WHERE local.Attending
                                                            UNION SELECT admit.DietaryRestriction AS Diet
                                                                  FROM admit
                                                                  WHERE admit.Attending
                                                          ORDER BY Diet DESC)
                                             val labels = {Diet = "Dietary Restriction"}
                                         end)

    (* Database tweaking, etc. *)
    val sendEmailsToLocals = requireAdmin;
            queryI1 (SELECT local.CsailId, local.Password
                     FROM local)
                    (fn r => Mail.send (Mail.to (r.CsailId ^ "@cs.cmu.edu") (Mail.from "yano@cs.cmu.edu" (Mail.subject "Your CMU CSD Open House Password" Mail.empty))) (r.CsailId ^ ": " ^ r.Password ^ "\nTo access the system, log in at http://visit.cs.cmu.edu/visit/index.") None)

(*
    val sendEmailsToAdmits = requireAdmin;
            queryI1 (SELECT admit.AdmitId, admit.AdmitName, admit.Email FROM admit)
                      (fn r => Mail.send (Mail.to r.Email (Mail.from "yano@cs.cmu.edu" (Mail.subject "CMU CSD Open House RSVP" Mail.empty))) ("http://visit.cs.cmu.edu/visit/Admits/main/" ^ (show r.AdmitId)) None)
*)


      

    val admin =
        requireAdmin;
        ia <- source "";
        ip <- source "";

        siteMode <- oneRowE1 (SELECT COUNT( * ) > 0
                              FROM visitStarted);
        siteMode <- source siteMode;

        schMode <- oneRowE1 (SELECT COUNT( * ) > 0
                             FROM schedulingStarted);
        schMode <- source schMode;

        admitsMasq <- queryX1 (SELECT admit.AdmitId, admit.AdmitName
                               FROM admit
                               ORDER BY admit.AdmitName)
                              (fn r => <xml>
                                <tr><td><a link={Admits.main r.AdmitId}>{[r.AdmitName]}</a></td></tr>
                              </xml>);

        pisMasq <- queryX1 (SELECT local.CsailId
                            FROM local
                            WHERE local.IsPI
                            ORDER BY local.CsailId)
                           (fn r => <xml>
                             <tr><td><a link={pi r.CsailId}>{[r.CsailId]}</a></td></tr>
                           </xml>);



        Theme.tabbed "CSD Open House Admin"
                  ((Some "Faculty & Students",
		  	 Ui.seq ((Ui.const <xml><p>Do not use this page to add local users. Instead, use the Import Faculty/Students tab.</p></xml>),
                    	 	EditLocal.ui)),
                    (Some "Email passwords to CMU users",
                      Ui.const <xml><p/><button class="btn btn-primary" value="Send Emails to all CMU users" onclick={fn _ => rpc sendEmailsToLocals}/></xml>),
(*                     (Some "Email access links to admits",
                      Ui.const <xml><p/><button class="btn btn-primary" value="Send Emails to all admits" onclick={fn _ => rpc sendEmailsToAdmits}/></xml>),
*)

                   (Some "Import Faculty/Students", Ui.const <xml>
                      <p>Enter one record per line, with fields separated by commas.  The field order is: <i>CS username</i>, <i>name</i>, <i>office</i>.</p>

                      <ctextarea source={ip} cols={20} class="form-control"/>
                      <button class="btn btn-primary"
                              value="Import PIs"
                              onclick={fn _ =>
                                          ip <- get ip;
                                          rpc (importPIs ip)}/>
                    </xml>),
                   (Some "Prospectives",
                    EditAdmit.ui),
                   (Some "Import Prospectives", Ui.const <xml>
                      <p>Enter one admit record per line, with fields separated by commas.  The field order is: <i>name</i>, <i>e-mail address</i>.</p>

                      <ctextarea source={ia} cols={20} class="form-control"/>
                      <button class="btn btn-primary"
                              value="Import Prospectives"
                              onclick={fn _ =>
                                          ia <- get ia;
                                          rpc (importAdmits ia)}/>
                    </xml>),
                   (Some "Meeting Times", EditTime.ui),
                   (Some "Meals", EditDinner.ui),
                   (Some "Mode", Ui.constM (fn ctx => <xml>
                     <dyn signal={md <- signal schMode;
                                  return (if md then <xml>
                                    <p>
                                    The site is in <b>meeting scheduling mode</b>.
                                    (That is, PIs should be allowed to tweak the schedule grid.)
                                    </p>
                                    <button class="btn btn-primary"
                                            value="Revert to pre-scheduling mode"
                                            onclick={fn _ =>
                                                        rpc startScheduling;
                                                        set schMode False}/>
                                  </xml> else <xml>
                                    <p>
                                      The site is in <b>pre-meeting scheduling mode</b>.
                                      (That is, PIs aren't seeing the meeting schedule yet.)
                                    </p>
                                    <button class="btn btn-primary"
                                            value="Enter scheduling mode"
                                            onclick={fn _ =>
                                                        rpc suspendScheduling;
                                                        set schMode True}/>
                                  </xml>)}/>

                     <hr/>

                     <dyn signal={md <- signal siteMode;
                                  return (if md then <xml>
                                    <p>
                                    The site is in <b>visit weekend mode</b>.
                                    (That is, I think the weekend has already begun.)
                                    </p>
                                    <button class="btn btn-primary"
                                            value="Revert to pre-weekend mode"
                                                      onclick={fn _ =>
                                                                  rpc startVisit;
                                                                  set siteMode False}/>
                                  </xml> else <xml>
                                    <p>
                                      The site is in <b>pre-visit weekend mode</b>.
                                      (That is, I think the weekend hasn't started yet.)
                                    </p>
                                    <button class="btn btn-primary"
                                            value="Enter weekend mode"
                                            onclick={fn _ =>
                                                        rpc suspendVisit;
                                                        set siteMode True}/>
                                  </xml>)}/>

                     <hr/>

                     <button class="btn btn-primary"
                             value="Schedule new meetings heuristically based on preferences"
                             onclick={fn _ => rpc scheduleMore}/><br/>
                     (Will not disturb any existing meetings.)
                   </xml>)),
                   (Some "Faculty/Student Masquerade",
                    Ui.const <xml>
                      <table class="bs3-table table-striped">
                        {pisMasq}
                      </table>
                    </xml>),
                   (Some "Prospective Student Masquerade",
                    Ui.const <xml>
                      <table class="bs3-table table-striped">
                        {admitsMasq}
                      </table>
                    </xml>),
                 (* (Some "Madness",
                   Madness.ui), *)
                  (Some "Lodging",
                   Lodging.ui),
                  (Some "No RSVP",
                   NoRsvp.ui),
                  (Some "All Dietary Restrictions",
                   Dietary.ui),
                  (Some "Faculty/Students (RSVPd)",
                   PiRsvp.ui),
                  (Some "Faculty/Students (No RSVP)",
                   PiNoRsvp.ui))


    structure InputLocal = InputStrings.Make(struct
                                                 val const = {}
                                                 con given = [CsailId = _]
                                                 con fixed = [LocalName = _, IsAdmin = _, IsPI = _,
                                                              Attending = _, FiveMinute = _, Password = _]
                                                 val tab = local
                                                 val chosenLabels = {PhoneNumber = "Mobile Phone#",
                                                                     DietaryRestriction = "Dietary Restriction",
                                                                     Office = "Office",
                                                                     Transport = "Transportation to dinner, if going (driving, shuttle with admits [if your dinner has a shuttle], ...)"}
                                                 val textLabel = "Personal information"
                                                 val amGiven = amHome
                                             end)

    (* Rank-and-file participant portal *)
    val main =
        user <- usernameLocal;
        key <- return {CsailId = user};
        
        Theme.simple "CSD Open House - Participant Portal"
                  (Ui.seq
                       (InputLocal.ui key,
                        Ui.h2 <xml>Meals</xml>,
                        Dinners.Home.ui key))
end

fun setIt v =
    setCookie localC {Value = v,
                      Expires = None,
                      Secure = False}

val cookieSetup =
    sc <- source "";

    Theme.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={sc}/>
        <button value="Set" onclick={fn _ => v <- get sc; rpc (setIt v)}/>
        </xml>)}


val doLogin (input) =  setCookie localC {Value = input.Username,
                          Secure = False,
                          Expires = None};
                       setCookie localPassword {Value = input.Password,
                          Secure = False,
                          Expires = None};


			  isAdmin <- authAdmin (input.Username, input.Password);
			  isLocalUser <- authLocal (input.Username, input.Password);
                      	  if isAdmin then redirect (url Locals.admin)
                      	     else if isLocalUser then redirect (url (Locals.pi ""))
                             	  else return <xml><body>Invalid username or password.</body></xml>


val index = 
	sc <- source "";
	sc2 <- source "";
	Theme.tabbed "CMU user login"
	{1 = (Some "Login",
		Ui.const <xml>
			<form>
			<p/>Username<textbox{#Username}/>
			<p/>Password<password{#Password}/>
			<submit action={doLogin} value="Submit"/>
			</form>
				</xml>)
	}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
(*)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={Locals.admin}>Admin</a></li>
  <li><a link={Locals.pi ""}>PIs</a></li>
  <li><a link={Locals.main}>Other locals</a></li>
  <li><a link={Admits.main 0}>Admits</a></li>
</body></xml>
*)

