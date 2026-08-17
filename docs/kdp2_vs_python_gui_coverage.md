# KOKO オリジナル(Winteracter GUI) vs Python GUI — コマンド/機能カバレッジ比較

生成日: 2026-08-12
比較元:
- オリジナル: /home/dino/KDP2/*.FOR の `INPUT=` / `WRITE(INPUT,*)` 発行箇所 + IDD_* ダイアログ 85個
- Python GUI: /home/dino/Koko/gui_py/*.py の `send_koko` 実装全件

## 判定基準
- **OK**: Python GUI のメニュー/ボタンから対応コマンドが send_koko 経由で実際に送信される
- **未実装**: メニュー項目はある（またはオリジナルに存在する）が send_koko を呼ばない、あるいはダイアログ自体が存在しない
- **部分**: 基本形のみ実装、派生形（RECT/ELIP/POLY/TILT 等）が未実装

---

## A. 解析グラフ（メニュー項目はあるが送信未実装）

| 機能 | オリジナル ダイアログ | オリジナル コマンド | Python GUI 状態 |
|------|----------------------|---------------------|-----------------|
| スポット図 | IDD_SPOT / SPOTENERGY / SPOTPLOT / SPOTWRITE | `SPOT RING` `SPOT RAND` `SPOT RECT` `SPD *` `SPDSAVE` | **実装済み** (SPOT RING+SPD+STATS FULL+OUT TP) — koko が gnuplot 出力、GUI 描画確認済み。**SpotDialog 追加**: パターン(RING/RECT/RAND)+本数+統計(FULL/MIN)+波長指定→SPD/SPD ACC+PLTSPD+SPDSAVE/SPDADD/SPDSTATS。KDP2 IDD_SPOT/SPOTGUI.FOR 相当 |
| 点列図 (DOTF) | IDD_DOTF | `DOTF` `PLTDOTF` `DIFLEICA NO,n` | **実装済み** (SPACE I/O+FAR+DOTF+PLTDOTF,,1)。**DotfDialog に分割**: Graphs>Optical Transfer Function>Diffraction (DOTF)... が設定ダイアログを開き、OKで SPACE I/O+FAR/NEAR+DOTF+PLTDOTF[ LEICA],,1+DRAW。Leicaはダイアログ内チェックで選択（メニューは1本に統一済み） |
| 幾何 OTF (GOTF) | IDD_GOTF | `GOTF` `PLTGOTF` `GEOLEICA NO,n` | **実装済み** (SPACE I/O+FAR+GOTF+PLTGOTF,1)。**GotfDialog に分割**: Graphs>Optical Transfer Function>Geometical (GOTF)... が設定ダイアログを開き、OKで SPACE I/O+FAR/NEAR+GOTF+PLTGOTF[ LEICA],1+DRAW。Leicaはダイアログ内チェックで選択（メニューは1本に統一済み） |
| PSF | IDD_PSF / PSFENERGY / PSFSTREAK / PSFSTREHL | `PSF` `PSFLOG` `PSFWRITE` `PSFROT` `STREAK *` `STREHL` `APSTREHL` | **実装済み** (PSFWRITE YES+PSFLOG+PSFPLOT YES+PSF,1+CAPFNOUT)。**PsfDialog に分割**: Graphs>Point Spread Function (PSF)... が設定ダイアログを開き、OKで NRD+PSFWRITE/PSFPLOT トグル+モード(PSF/PERFECT/PERFNOOB)+波長→PSF,<wav>+CAPFNOUT |
| 複素瞳関数 (CAPFN) | IDD_CAPFN | `CAPFNNRD` `CAPFN` `CAPGRID` `WAMAP` `AMAP` `FITZERN` `LISTOPD` `LISTZERN` `LISTREPT` `CAPFNROT` `PLOT CAPFNOPD/CAPFNAPD` `CAPFNOUT/IN/ADD/CLR` | **実装済み**（CapfnDialog: NRD(偶数強制)+モード(CAPFN/PERFECT/SILENT)+波長解析(CAPGRID/WAMAP/AMAP/FITZERN)+一覧(LISTOPD/LISTZERN/LISTREPT)+OPD/振幅プロット(CAPFNROT+PLOT CAPFNOPD/CAPFNAPD[,min,max])+瞳ファイル操作(CAPFNOUT/IN/ADD/CLR)。KDP2 IDD_CAPFN/RAYS.INC 相当） |
| 歪曲/非点/像面湾曲 (DISAST) | IDD_DISAST | `FLDCV,<orient>,,<n>` `AST,<orient>,,<n>` `DIST,<orient>,,<n>` `FISHDIST,<orient>,,<n>` `PLTFLDCV` `PLTAST` `PLTDIST` `PLTFDIST` | **実装済み**（旧DisastDialogを3分割: FldcvDialog/AstDialog/DistDialog。Graphsメニューの Field Curvature(FLDCV).../Astigmatism(AST).../Distortion(DIST)... が各設定ダイアログを開き、OKで設定→描画。方位(0/90度)+フィールド点数(10-50)+プロットチェック。KDP2 IDD_DISAST/RAYS.INC 相当） |
| レンズ操作 (FLIP/SCALE/ZERO/OTHER) | IDD_FLIP / IDD_SCALE / IDD_ZERO / IDD_OTHER | `FLIP,<s>,<e>` `SC/WSC[,FY],<factor>,<s>,<e>` `U L+CHG+ZERO+EOS` `U L+CHG+REAL/PARAX+FOOTBLOK+NODUM+SPGR+PRICE+INR/INRD+RAYERROR+LBL+COATING+EOS` | **実装済み**（LensOpsDialog: FLIP=開始/終了面→FLIP,<s>,<e>。SCALE=SC/WSC/SC FY/WSC FY+倍率+開始/終了面。ZERO=面番号→U L+CHG+ZERO+EOS+RTG ALL。OTHER=面番号+トレース種別(REAL/PARAX)+FOOTBLOK/NODUM トグル+SPGR/PRICE/INR/RAYERROR/LBL/COATING(任意)→U L+CHG+各コマンド+EOS+RTG ALL。SRADIUS/SCURVATURE/STHICKNESS は既存テーブルがカバー、ROO/CCR は KDP2 にハンドラなしで除外。koko が全コマンドを PTY で受付確認済み: FLIP 実行確認+OTHER 系全受理） |
| 複数開口/遮蔽 (MCLAP/MCOBS/CLAPS) | IDD_CLAPS / IDD_MCLAP / IDD_MCOBS | `MULTCLAP,<n>,<x>,<y>[,<gam>]` `MULTCLAP DELETE` `MULTCOBS,<n>,<x>,<y>[,<gam>]` `MULTCOBS DELETE` | **実装済み**（MultiApertureDialog: 面番号+インスタンス番号(1-1000)+X/Yオフセット+回転(任意)→U L+CHG+MULTCLAP/MULTCOBS+EOS+RTG ALL。Delete all ボタン→MULTCLAP/MULTCOBS DELETE。MULTCLAP は既存 CLAP、MULTCOBS は既存 COBS が必要。koko が PTY で受付確認済み: MULTCLAP 受理+DELETE 正常+MULTCOBS は COBS 無しで正しく拒否。ldm8.f のコピーペーストバグ修正済み: MULT_COBS のエラーメッセージが "MULTCLAP" になっていたのを "MULTCOBS" に修正） |
| レンズ出力 (LENOACC/LENOCV) | IDD_LENOACC / IDD_LENOCV | `OUT FILE <f>` `LENO AC` `LENO CV` `OUT TP` | **実装済み**（LENO CV=既存 GUI の Export Code-V。LENO AC=新規追加: File メニュー "Export Lens (LENO AC)..."→OUT FILE+LENO AC+OUT TP。koko が PTY で LENO AC のレンズデータ出力を確認済み） |
| About (IDD_ABOUT) | IDD_ABOUT | — | **実装済み**（File メニュー "About Koko..."→QMessageBox.about でバージョン/ライセンス表示。KDP2 IDD_ABOUT 相当） |
| EDITFONT / COMMAND_INSTRUCTOR / EDITCFG | IDD_EDITFONT / IDD_COMMAND_INSTRUCTOR / IDD_EDITCFG | — | **実装不可**（koko にフォント編集・コマンドインストラクタ・設定編集の相当機能なし。KDP2 のみ） |
| ファン図（横/縦/対角/OPD/CD/LA） | IDD_FAN1 | `FANS XFAN` `FANS YFAN` `FANS XYFAN` `FANS NFAN` `FANS PFAN` (+CD/LA/OPD) | **実装済み** (FANS <qualifier> — KDP2 RIMS サブルーチン経由。PTY/GUI で PNG 生成確認済み) |
| 単一光線追跡 | IDD_RAY | `RAY` `FOB` `AIMRAY ON` `PRXYZ ALL` `DRAWFAN` | **実装済み** (RayDialog: [Trace]→FOB+RAY+PRXYZ ALL（テキスト）、[Plot Fan]→FANS XFAN（図）。GUI 動作確認済み。koko の RAY 単体は追跡結果をテキスト出力しないため PRXYZ ALL で主光線座標を表示) |
| 近軸 (PARAXIAL) | IDD_PARAX1/2/3 | `FCHY ALL` `FCHX ALL` `PCD3 ALL` `SCD3 ALL` `PRXYZ ALL` `PRR ALL` | **実装済み** (menuParaxial 配下の各項目 → slot_text で対応コマンド送信。koko がテキスト出力。GUI 動作確認済み) |
| 表示制御 (VIE) | IDD_VIE | `VIE` `VIE XZ/XY/ORTHO` `VIEVIG ON/OFF` `VIESYM ON/OFF` `VIEOFF` `VIECO` `PLOT VIEW` `PLOT NOTE/PEN/FRAME/AXIS/UPLOT` | **実装済み**（VieDialog: 視点+XZ/XY/ORTHO+スケール+ビネット/対称トグル→VIEVIG/VIESYM+VIE <type>,<factor>。koko 受付確認済み。PlotDetailDialog: PLOT FRAME(座標指定可/既定0 0 10000 7000)+PLOT AXIS+PNOTE <text>+PLOT NOTE x y+PLOT PEN x y state+PLOT UPLOT xr1 xr2 yr1 yr2 を送信後 DRAW で drawcmd.gpl 再生成+GUI 描画。koko が全コマンドを PTY で受付確認済み） |

### A 群 実装メモ（2026-08-12 コミット c88725c）
- SPOT/DOTF/GOTF/PSF は「コマンド列修正のみ」で動作。koko が $HOME/gnuplot/drawcmd.gpl を書き、既存の slot_plot → gnuplot → PNG 描画基盤が its まま機能（KokoMainWindow 経由で drawcmd.gpl 更新を確認済み）。
- FAN 図は koko が事前の光線追跡データファイル（RF）を要求し、コマンドラインから PLTXFAN 単独では "NO ACTION TAKEN"。オリジナルは IDD_FAN1 ダイアログ内で光線セットを構築してから PLTXFAN を呼ぶ。専用ダイアログ作成が必要。
- RAY/PARAXIAL はメニュー項目自体が Python GUI に存在しない。新規ダイアログ作成が必要。

## B. 完全に欠落している編集/設定機能

| 機能 | オリジナル ダイアログ | オリジナル コマンド | Python GUI 状態 |
|------|----------------------|---------------------|-----------------|
| アポディゼーション | IDD_APOD | `APOD GAUSS` `APOD NONE` | **実装済み** (ApodDialog → APOD GAUSS,<val> / APOD NONE。GUI 動作確認済み) |
| 回折設定 | IDD_DIFSET | `DIFFOB` `DIFRAY` `DIFLEICA` | OK（実装済み） |
| 絞り面 (STOP) | IDD_STOPSURF | `ASTOP` `ASTOP EN` `ASTOP EX` `ASTOP ENEX` | **実装済み**（StopDialog: 面番号+瞳孔調整オプション→U L+CHG+ASTOP[ EN/EX/ENEX]+EOS+RTG ALL。koko 受付確認済み） |
| 参照面 (REF) | IDD_REFSSURF | `REFS` | **実装済み**（RefDialog: 面番号+回転角→U L+CHG+REFS <rot>+EOS+RTG ALL。koko 受付確認済み） |
| 偏心 (DEC) | IDD_DEC | `DEC` `DEC 0 0 0` | **実装済み**（DecDialog: 面番号+X/Y/Z→U L+CHG+DEC X Y Z+EOS+RTG ALL。koko 受付確認済み。KDP2 の DEC,Y,X,Z 順序を X/Y/Z 表示にマップ） |
| 開口/遮蔽（CLAP/COBS 派生形） | IDD_APECIRC/APERECT/APEELIP/APERCTK/APEPOLY + IDD_APECIRC2/APERECT2/APEELIP2/APERCTK2/APEPOLY2 + ERASE系 | `CLAP`(円) `CLAP RECT` `CLAP ELIP` `CLAP RCTK` `CLAP POLY` `CLAP ERASE` `CLAP TILT` `CLAPD` `COBS`(円) `COBS RECT` `COBS ELIP` `COBS RCTK` `COBS POLY` `COBS ERASE` `COBS TILT` `COBSD` | **実装済み**（ApertureDialog=CLAP 円/矩形/楕円/RCTK/POLY(辺数指定)/ERASE/CLAPD 全形状、ObscurationDialog=COBS 円/矩形/楕円/RCTK/POLY/ERASE/COBSD 全形状。koko が全派生形を PTY で受付確認済み） |
| 傾斜 (TILT 系列) | IDD_TILTS / IDD_TILT / IDD_TILTAUTO / IDD_TILTBEN / IDD_TILTRET / IDD_TILTDAR / IDD_TILTREV | `TILT`(基本3軸) `TILT AUTO` `TILT DARD` `TILT BEND` `TILT REV` `RTILT` `TILTD` | **実装済み**（TiltDialog: タイプ選択＋基本はα/β/γ入力。U L+CHG+対応TILTコマンド+EOS+RTG ALL。koko 受付確認済み） |
| 非球面/トーリック (ASPH) | IDD_ASPH | `ASPH` `CC` `AC`〜`AL`(2〜20次) `YTORIC` `XTORIC` `RDTOR` `CVTOR` `CCTOR` `TASPH` `ADTOR`〜`AGTOR` | **実装済み**（AsphDialog: 非球面モード=CC+AC/AD/AE/AF/AG/AH/AI/AJ/AK/AL 10係数→U L+CHG+ASPH+CHG+CC+各係数+EOS。トーリックモード=YTORIC/XTORIC+RDTOR/CVTOR+CCTOR。koko が全コマンドを PTY で受付確認済み。注: AC 項は非平面では無視される警告は正常） |
| 回折格子/アレイレンズ (GRTARRAY) | IDD_GRTARRAY | `GRT` `GRO` `GRS` `GRX` `GRY` `GRZ` `GRTD` `ARRAY ODD/EVEN` `ARRAYD` | **実装済み**（GrtArrayDialog: 格子割当=GRT+GRO/GRS/GRX/GRY/GRZ+EOS、格子削除=GRTD、アレイ割当=ARRAY ODD/EVEN,dx,dy、アレイ削除=ARRAYD。koko が全コマンドを PTY で受付確認済み） |
| 特殊面 (SPSRF) | IDD_SPSRF | `U SP` `SPECIAL,<surf>,<type>` `EOS` | **実装済み**（SpsrfDialog: 面番号+特殊タイプ(1〜24)→U SP+SPECIAL,<surf>,<type>+EOS+RTG ALL。koko が PTY で受付確認済み。タイプ18は単純球面/円錐鏡のみ有効） |
| 黒体放射 (BB) | IDD_BB | `RADUNITS WATTS/PHOTONS` `WIEN P,<T>` `STEFBOLT P,<T>,<λ上限>,<λ下限>` `PLANK P,<T>,<λ>` | **実装済み**（BbDialog: 単位ラジオ(WATTS/PHOTONS)+3計算ボタン(WIEN=ピーク波長/STEFBOLT=積分放射/PLANK=分光放射)。各ボタンが RADUNITS <unit>→対応コマンドを送信、koko がテキスト結果を msgView に出力。ダイアログは開いたまま連続計算可。PTY で全コマンドの正しい物理出力を確認済み: WIEN 5000K→0.5796µm、STEFBOLT→2245.6 W/cm²、PLANK→3993.4 W/cm²-µm） |
| レイ設定/解析補助 (RAYSETTINGS/FIRD/ISTAT/FAIL) | IDD_RAYSETTINGS / IDD_FIRD / IDD_ISTAT / IDD_FAIL | `SURTOL` `AIMTOL` `CAIMTOL` `NRAITR` `FIRD,NW1,NW2` `SPD ISTAT/IPSTAT,<J>,<start>,<end>,<del>` `FAIL[,<s1>,<s2>]` `FAILACC` | **実装済み**（RayAuxDialog: 4グループを1ダイアログに統合。RAYSETTINGS=SURTOL/AIMTOL/CAIMTOL/NRAITR の表示・設定(空欄=現在値表示)。FIRD=近軸EFL/BFL/FFL。ISTAT=FOB→SPD ISTAT/IPSTAT で入射角/屈折角統計。FAIL=FOB→SPOT RING→SPD→FAIL/FAILACC で失敗レイ数。全て CMD レベルのテキスト出力で msgView に表示、ダイアログは開いたまま連続実行可。PTY で全コマンドの正しい出力を確認済み: FIRD→EFL 116.2mm、ISTAT→角度ヒストグラム、FAIL→FAILED RAYS 集計） |
| パラメータ拾い (PIKUP) | IDD_PIKSLV / IDD_PIKED1/2/3 | `PIKUP *` (CV/RD/CC/TH/AD/AE/AF/AG/TOR/ALPHA/BETA/GAMMA/XD/YD/GLASS/PRO/PIVX/Y/Z 等44種) `PIKD *` `SLV` | **実装済み** (PikupDialog: 面番号+種別+値 → U L + PIKUP <TYPE>,<surf>,<val> + EOS + RTG ALL。koko が受付確認済み。SolveDialog: 10種のソルブ(PY/PX,PCY/PCX,PUY/PUX,PIY/PIX,PUCY/PUCX,PICY/PICX,COCY/COCX,CAY/CAX,APY/APX,APCY/APCX)×Y/X平面+目標値 → U L+CHG+<SOLVE> <val>+EOS+RTG ALL。PIKD(面の全ピックアップ削除)・SLV ALL(全ソルブ一覧)ボタン付き。koko が PTY で受付確認済み。注: 主光線系ソルブは絞面前の面では物理制約で拒否される) |
| ピボット軸 (PIVAXIS) | IDD_PIVAX | `PIVAXIS NORMAL` `PIVAXIS VERTEX` `PIVOT,X,Y,Z` `PIVAXIS ?` | **実装済み**（PivaxisDialog: NORMAL または VERTEX(座標指定)、または表示のみ(PIVAXIS ?)→U L+CHG+PIVAXIS NORMAL / PIVAXIS VERTEX+PIVOT,X,Y,Z / PIVAXIS ?+EOS+RTG ALL。koko 受付確認済み） |
| ガラスライブラリ | IDD_GLASSP / EDGLASS / LLIB | `LIB GET/PUT/DEL` `LENADD` `LIBSAVE` `LIBREST` `GLASSP` `FINDGLASS` | **実装済み**（FINDGLASS GUI: Material(nk)ダイアログの Model ページに FINDGLASS 領域を追加。Index n / Abbe V から全カタログ(858ガラス)の中で最寄り5件を検索表示(glassmap.find_nearest_glasses)、ダブルクリックで実ガラスに置換。Lens Library LIB GET/PUT/DEL 実装済み。koko が LIB GET/PUT/DEL をサポート確認済み。LIB REST/SAVE/LIST は koko 未サポート、GLASSP は送信のみ） |
| マクロ | IDD_MACRO | `MACSAVE` `MACREST` `MAC_EDIT` `MACROOPT` | 部分（MacroDialog: ライブラリ初期化 IMF+PROCEED（~/KODS/LIBMAC/ 未作成時のみ有効）、実行(MACRO name)/削除(MDEL name)/編集(MACED name→mac>モード)。koko が ~/KODS/LIBMAC/MAC.DAT を要求し、IMF+PROCEED で初期化できることを確認済み。MACED 編集時の複数行入力+MACSAVE は koko 側 mac> モードで手動扱い） |
| 有効径 (APERTURE/CLAP) | IDD_APECIRC 系 | `CLAP` `COBS` `APCX` `APCY` `APX` `APY` `CAPFN` `CAPFNOUT` | **実装済み**（円形 CLAP: ApertureDialog → U L + CHG <surf> + CLAP <rad> <xdecenter> <ydecenter> 0 0 + EOS + RTG ALL。koko 受付確認済み。APCX/APCY/APX/APY/CAPFN は未実装） |
| 面種別 (SURTYPE) | （KDP2 にも設定ダイアログなし / 表示コマンドのみ） | `SURTYPE <surface>` `SURTYPE ALL` | **実装済み**（SurtypeDialog: 面番号またはALL→SURTYPE <surf>/ALL 送信。koko が REAL/PARAXIAL をテキスト出力。GUI 経由で koko 受付確認済み。msgView 表示は既知の非同期読み取りタイミング問題あり＝PARAXIAL/RAY と同じ） |
| コーティング (COATING) | — | `COATING <n>` `COATING ?` | **実装済み**（CoatingDialog: 面番号+コーティング番号(0=none)、または表示のみ(COATING ?)→U L+CHG+COATING <n>/?+EOS+RTG ALL。koko 受付確認済み） |
| 多構成 (CONFIGS) | IDD_CONFIGS | `CONFIGS ALL` `UPDATE LENS` `CFG` | **実装済み** (menuLensData → CONFIGS ALL 配線済み。GUI 動作確認済み) |
| 非シーケンシャル (NSS) | — | `NSSNEW` `NSSUNITS` `NSSWV` `UNIVERSE` `OBJECT` `ONAME` `NSSSAVE` `NSSREST` `NSSTRACE` `NSSLIST` `NSSDEL` | **実装済み**（NssDialog: 新規作成/単位/波長/ユニバース/オブジェクト定義/レイトレース/一覧/保存/復元/削除ボタン。koko が NSSNEW でデータベース作成後、全 NSS コマンドをサポートすることを PTY で確認済み） |
| 公差 (TOLERANCING) | menuTolerancing | `TEL YES` 等 | **実装済み**（ToperDialog: TVARモードで公差変数(TH/RD_FR/CV_FR/CC/AD/AE/AF/AG/XD/YD/PIVX/Y/Z)定義→TOPERモードでオペランド(FUNCxx)定義→SENSI/MONTE解析。koko が TVAR→tvb>、TOPER→top>、SENSI/MONTE を完全サポートし、TVAR+TOPER+SENSI で感度解析レポートを出力することを PTY で確認済み。TOLNRD でグリッド設定） |
| ガラスマップ (GLASS MAP) | menuGraphs | — | **実装済み**（GlassMapDialog: カタログ選択(CDGM/Schott/Hoya/Ohara/Hikari/Sumita)→AGF/CSVから Nd,Vd を抽出→gnuplot(pngcairo)で n-v 散布図を PNG 出力→GlassMapWindow で表示。クリックするとピクセル座標を (n,v) に逆変換し、最寄りのガラス名・n・v を msgView に表示。858 ガラスを読み込み、N-BK7 で逆変換精度を確認済み） |
| イメージボケ (IMAGE BLUR) | — | `COLOR RGB` `IIMAGEN` `IOBJECTD` `OFROMBMP` `IMTRACE2`/`IMTRACE3` `PLTIMG` | **実装済み**（ImageBlurDialog: BMP選択→~/KODS/KOBJ.BMP にコピー→`COLOR RGB`+`IIMAGEN`+`IOBJECTD`+`OFROMBMP KOBJ`+`IMTRACE2`(Single PSF)または`IMTRACE3`(Full PSF per point)+`PLTIMG <trim>` を送信→`~/KODS/PLOTBMP.BMP` をポーリング表示。**計算コアは KDP2 の IMAGE1.FOR(FULLIMAGING) と極力同一コード**（Src/raytrace/image.f は KDP2 IMAGE1.FOR の移植；PSFTOIMG も KDP2 準拠の「保持 PSF を各点に畳み込み」に修正済み）。PTY で IMTRACE2/IMTRACE3 とも `IMTRACE? TRACING DONE` まで到達し PLOTBMP.BMP 生成を確認済み。注：koko の EXIT コマンドは空レンズ環境で LENSTEXT.DAT アクセスクラッシュする既知問題あり（ImageBlur 自体は成功） |

## C. 実装済み（OK）

| 機能 | コマンド |
|------|---------|
| レンズ新規/保存/読込 | `LENS` `LI` `LENSSAVE` `LENSREST` `RTG ALL` |
| Zemax/CODE-V 入出力 | `ZMX2PRG` `CV2PRG` `LENO ZMX` `LENO CV` `OUT FILE` |
| 面編集（挿入/削除/ガラス/曲率/厚み） | `U L` `CHG` `INS` `DEL` `RD` `CV` `TH` `GLASS` `FINDGLASS` `EOS` |
| 最適化 | `MERIT` `FLCLTH` `VARIABLES` `VB` `OPRD` `ITER` `ITER FULL` `IT P` `PFIND` `ROBB` |
| 基本表示 | `VIE` `VIE XZ` `PLOT` |
| 焦点/入射角 | `PY` `SCY FANG` |
| アポダイゼーション設定 | `APOD`（actionApod 経由、部分的） |
| 回折設定 | `DIFFOB` `DIFRAY` `DIFLEICA`（actionDifset 経由、OK） |

---

## 優先実装候補（ユーザー確認待ち）
1. 解析グラフ: SPOT / DOTF / GOTF / PSF / FAN / RAY / PARAXIAL — 最も目に見える不足
2. 高度編集: PIKUP / PIVAXIS / APERTURE / COATING / CONFIGS
3. ユーティリティ: マクロ / ガラスライブラリ / NSS / 公差

※ 各機能の正確なコマンド書式は /home/dino/KDP2 の対応 FOR（GUICODE.FOR, OPTIMIZE.FOR, SPOTGUI.FOR, DOTFGUI.FOR, GOTFGUI.FOR, PSFGUI.FOR, VARIABLES.FOR 等）を参照。
