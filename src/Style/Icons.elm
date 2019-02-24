module Style.Icons exposing (..)

import Element as E
import Html
import Octicons as O
import Style.Palette exposing (..)


defOptions =
    O.defaultOptions


size =
    O.size


color : E.Color -> O.Options -> O.Options
color c =
    let
        rgba =
            E.toRgb c

        toVal v =
            v
                * 255
                |> round
                |> String.fromInt

        colorStr =
            "rgba("
                ++ toVal rgba.red
                ++ ","
                ++ toVal rgba.green
                ++ ","
                ++ toVal rgba.blue
                ++ ","
                ++ toVal rgba.alpha
                ++ ")"
    in
    O.color colorStr


iconElement : (O.Options -> Html.Html msg) -> (O.Options -> E.Element msg)
iconElement f =
    \o ->
        E.el []
            (E.html <| f o)


alert =
    iconElement O.alert


archive =
    iconElement O.archive


arrowBoth =
    iconElement O.arrowBoth


arrowDown =
    iconElement O.arrowDown


arrowLeft =
    iconElement O.arrowLeft


arrowRight =
    iconElement O.arrowRight


arrowSmallDown =
    iconElement O.arrowSmallDown


arrowSmallLeft =
    iconElement O.arrowSmallLeft


arrowSmallRight =
    iconElement O.arrowSmallRight


arrowSmallUp =
    iconElement O.arrowSmallUp


arrowUp =
    iconElement O.arrowUp


beaker =
    iconElement O.beaker


bell =
    iconElement O.bell


bold =
    iconElement O.bold


book =
    iconElement O.book


bookmark =
    iconElement O.bookmark


briefcase =
    iconElement O.briefcase


broadcast =
    iconElement O.broadcast


browser =
    iconElement O.browser


bug =
    iconElement O.bug


calendar =
    iconElement O.calendar


check =
    iconElement O.check


checklist =
    iconElement O.checklist


chevronDown =
    iconElement O.chevronDown


chevronLeft =
    iconElement O.chevronLeft


chevronRight =
    iconElement O.chevronRight


chevronUp =
    iconElement O.chevronUp


circleSlash =
    iconElement O.circleSlash


circuitBoard =
    iconElement O.circuitBoard


clippy =
    iconElement O.clippy


clock =
    iconElement O.clock


cloudDownload =
    iconElement O.cloudDownload


cloudUpload =
    iconElement O.cloudUpload


code =
    iconElement O.code


comment =
    iconElement O.comment


commentDiscussion =
    iconElement O.commentDiscussion


creditCard =
    iconElement O.creditCard


dash =
    iconElement O.dash


dashboard =
    iconElement O.dashboard


database =
    iconElement O.database


desktopDownload =
    iconElement O.desktopDownload


deviceCamera =
    iconElement O.deviceCamera


deviceCameraVideo =
    iconElement O.deviceCameraVideo


deviceDesktop =
    iconElement O.deviceDesktop


deviceMobile =
    iconElement O.deviceMobile


diff =
    iconElement O.diff


diffAdded =
    iconElement O.diffAdded


diffIgnored =
    iconElement O.diffIgnored


diffModified =
    iconElement O.diffModified


diffRemoved =
    iconElement O.diffRemoved


diffRenamed =
    iconElement O.diffRenamed


ellipses =
    iconElement O.ellipses


ellipsis =
    iconElement O.ellipsis


eye =
    iconElement O.eye


eyeClosed =
    iconElement O.eyeClosed


file =
    iconElement O.file


fileBinary =
    iconElement O.fileBinary


fileCode =
    iconElement O.fileCode


fileDirectory =
    iconElement O.fileDirectory


fileMedia =
    iconElement O.fileMedia


filePdf =
    iconElement O.filePdf


fileSubmodule =
    iconElement O.fileSubmodule


fileSymlinkDirectory =
    iconElement O.fileSymlinkDirectory


fileSymlinkFile =
    iconElement O.fileSymlinkFile


fileText =
    iconElement O.fileText


fileZip =
    iconElement O.fileZip


flame =
    iconElement O.flame


fold =
    iconElement O.fold


foldDown =
    iconElement O.foldDown


foldUp =
    iconElement O.foldUp


gear =
    iconElement O.gear


gift =
    iconElement O.gift


gist =
    iconElement O.gist


gistSecret =
    iconElement O.gistSecret


gitBranch =
    iconElement O.gitBranch


gitCommit =
    iconElement O.gitCommit


gitCompare =
    iconElement O.gitCompare


gitMerge =
    iconElement O.gitMerge


gitPullRequest =
    iconElement O.gitPullRequest


globe =
    iconElement O.globe


grabber =
    iconElement O.grabber


graph =
    iconElement O.graph


heart =
    iconElement O.heart


history =
    iconElement O.history


home =
    iconElement O.home


horizontalRule =
    iconElement O.horizontalRule


hubot =
    iconElement O.hubot


inbox =
    iconElement O.inbox


info =
    iconElement O.info


issueClosed =
    iconElement O.issueClosed


issueOpened =
    iconElement O.issueOpened


issueReopened =
    iconElement O.issueReopened


italic =
    iconElement O.italic


jersey =
    iconElement O.jersey


kebabHorizontal =
    iconElement O.kebabHorizontal


kebabVertical =
    iconElement O.kebabVertical


key =
    iconElement O.key


keyboard =
    iconElement O.keyboard


law =
    iconElement O.law


lightBulb =
    iconElement O.lightBulb


link =
    iconElement O.link


linkExternal =
    iconElement O.linkExternal


listOrdered =
    iconElement O.listOrdered


listUnordered =
    iconElement O.listUnordered


location =
    iconElement O.location


lock =
    iconElement O.lock


logoGist =
    iconElement O.logoGist


logoGithub =
    iconElement O.logoGithub


mail =
    iconElement O.mail


mailRead =
    iconElement O.mailRead


mailReply =
    iconElement O.mailReply


markGithub =
    iconElement O.markGithub


markTor =
    iconElement O.markTor


markTwitter =
    iconElement O.markTwitter


markdown =
    iconElement O.markdown


megaphone =
    iconElement O.megaphone


mention =
    iconElement O.mention


milestone =
    iconElement O.milestone


mirror =
    iconElement O.mirror


mortarBoard =
    iconElement O.mortarBoard


mute =
    iconElement O.mute


noNewline =
    iconElement O.noNewline


note =
    iconElement O.note


octoface =
    iconElement O.octoface


organization =
    iconElement O.organization


package =
    iconElement O.package


paintcan =
    iconElement O.paintcan


pencil =
    iconElement O.pencil


person =
    iconElement O.person


pin =
    iconElement O.pin


plug =
    iconElement O.plug


plus =
    iconElement O.plus


plusSmall =
    iconElement O.plusSmall


primitiveDot =
    iconElement O.primitiveDot


primitiveSquare =
    iconElement O.primitiveSquare


project =
    iconElement O.project


pulse =
    iconElement O.pulse


question =
    iconElement O.question


quote =
    iconElement O.quote


radioTower =
    iconElement O.radioTower


reply =
    iconElement O.reply


repo =
    iconElement O.repo


repoClone =
    iconElement O.repoClone


repoForcePush =
    iconElement O.repoForcePush


repoForked =
    iconElement O.repoForked


repoPull =
    iconElement O.repoPull


repoPush =
    iconElement O.repoPush


rocket =
    iconElement O.rocket


rss =
    iconElement O.rss


ruby =
    iconElement O.ruby


screenFull =
    iconElement O.screenFull


screenNormal =
    iconElement O.screenNormal


search =
    iconElement O.search


server =
    iconElement O.server


settings =
    iconElement O.settings


shield =
    iconElement O.shield


signIn =
    iconElement O.signIn


signOut =
    iconElement O.signOut


smiley =
    iconElement O.smiley


squirrel =
    iconElement O.squirrel


star =
    iconElement O.star


stop =
    iconElement O.stop


sync =
    iconElement O.sync


tag =
    iconElement O.tag


tasklist =
    iconElement O.tasklist


telescope =
    iconElement O.telescope


terminal =
    iconElement O.terminal


textSize =
    iconElement O.textSize


threeBars =
    iconElement O.threeBars


thumbsdown =
    iconElement O.thumbsdown


thumbsup =
    iconElement O.thumbsup


tools =
    iconElement O.tools


trashcan =
    iconElement O.trashcan


triangleDown =
    iconElement O.triangleDown


triangleLeft =
    iconElement O.triangleLeft


triangleRight =
    iconElement O.triangleRight


triangleUp =
    iconElement O.triangleUp


unfold =
    iconElement O.unfold


unmute =
    iconElement O.unmute


unverified =
    iconElement O.unverified


verified =
    iconElement O.verified


versions =
    iconElement O.versions


watch =
    iconElement O.watch


x =
    iconElement O.x


zap =
    iconElement O.zap
