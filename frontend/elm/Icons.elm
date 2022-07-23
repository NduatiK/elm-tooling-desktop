module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Element exposing (html)


logo : Element.Element msg
logo =
    Element.el [] <|
        html <|
            svg
                [ width "65px"
                , height "26px"
                , viewBox "0 0 65 26"
                , version "1.1"
                ]
                [ g
                    [ id "bookay-logo"
                    , fill "currentColor"
                    ]
                    [ Svg.path
                        [ d "M4.70313 5.85156L5.09961 5.75586C5.1543 5.75586 5.18164 5.80827 5.18164 5.91309C5.18164 6.01791 5.02441 6.1888 4.70996 6.42578C4.39551 6.66276 4.01725 6.78125 3.5752 6.78125C3.13314 6.78125 2.78678 6.65365 2.53613 6.39844C2.28548 6.14323 2.16016 5.81055 2.16016 5.40039C2.16016 4.53451 2.72526 3.67546 3.85547 2.82324C4.98568 1.97103 6.37565 1.28744 8.02539 0.772461C9.67513 0.257487 11.2565 0 12.7695 0C14.9844 0 16.6318 0.360026 17.7119 1.08008C18.792 1.80013 19.332 2.76628 19.332 3.97852C19.332 5.528 18.2565 7.02734 16.1055 8.47656C14.9753 9.24219 13.7539 9.85287 12.4414 10.3086C13.8815 10.4635 15.0299 10.9284 15.8867 11.7031C16.5339 12.2865 16.8574 13.0293 16.8574 13.9316C16.8574 14.834 16.4268 15.7523 15.5654 16.6865C14.7041 17.6208 13.5579 18.3955 12.127 19.0107C10.696 19.626 9.23307 19.9336 7.73828 19.9336C6.01563 19.9336 4.7487 19.5553 3.9375 18.7988C3.57292 18.4525 3.39062 18.1449 3.39063 17.876C3.39062 17.6071 3.45898 17.3223 3.5957 17.0215C2.93034 18.2246 2.44271 19.1315 2.13281 19.7422C1.99609 20.0156 1.72038 20.1523 1.30566 20.1523C0.89095 20.1523 0.569661 20.043 0.341797 19.8242C0.113932 19.6055 0 19.3594 0 19.0859C0 18.8125 0.0410156 18.571 0.123047 18.3613C0.533203 17.2858 1.98014 14.7679 4.46387 10.8076C6.94759 6.84733 8.46745 4.55273 9.02344 3.92383C9.3151 3.56836 9.97591 3.39063 11.0059 3.39063C11.416 3.39063 11.6211 3.48177 11.6211 3.66406C11.6211 3.80078 11.015 4.82617 9.80273 6.74023C8.59049 8.6543 7.95703 9.6569 7.90234 9.74805C9.16927 9.38346 10.4658 8.88444 11.792 8.25098C13.1182 7.61751 14.2757 6.89974 15.2646 6.09766C16.2536 5.29557 16.748 4.57096 16.748 3.92383C16.748 2.85742 15.5905 2.32422 13.2754 2.32422C10.8509 2.32422 8.51758 2.9668 6.27539 4.25195C5.7194 4.57096 5.29557 4.86263 5.00391 5.12695C4.71224 5.39128 4.56641 5.57813 4.56641 5.6875C4.56641 5.79688 4.61198 5.85156 4.70313 5.85156L4.70313 5.85156ZM7.16406 12.3867C7 12.3867 6.81771 12.1953 6.61719 11.8125C5.7513 13.1979 4.8444 14.7383 3.89648 16.4336C4.32487 15.8411 4.94922 15.5449 5.76953 15.5449C6.31641 15.5449 6.58984 15.6133 6.58984 15.75C6.58984 15.8047 6.5306 15.8548 6.41211 15.9004C6.04753 16.0371 5.86523 16.2422 5.86523 16.5156C5.86523 17.1719 6.5625 17.5 7.95703 17.5C9.84375 17.5 11.5527 16.8438 13.084 15.5313C13.9043 14.8385 14.3145 14.2324 14.3145 13.7129C14.3145 13.3939 13.9818 13.043 13.3164 12.6602C12.5417 12.2318 11.3613 12.0176 9.77539 12.0176C8.67253 12.0176 7.96615 12.0791 7.65625 12.2021C7.34635 12.3252 7.18229 12.3867 7.16406 12.3867L7.16406 12.3867Z"
                        , id "B"
                        , fill "currentColor"
                        , stroke "none"
                        ]
                        []
                    , Svg.path
                        [ d "M57.4453 21.8008C57.4063 21.8398 57.2871 22.0371 57.0879 22.3926C56.8887 22.748 56.6758 23.1074 56.4492 23.4707C56.2227 23.834 55.9434 24.1797 55.6113 24.5078C55.2793 24.8359 55.002 25.0137 54.7793 25.041C54.5566 25.0684 54.4141 25.082 54.3516 25.082C53.6328 25.082 53.2734 24.7188 53.2734 23.9922C53.2734 23.1406 53.7148 22.1445 54.5977 21.0039C54.9336 20.5742 55.3555 20.1113 55.8633 19.6152C56.3711 19.1191 56.6484 18.8379 56.6953 18.7715C56.7422 18.7051 57.3281 17.6094 58.4531 15.4844C57.7031 16.3047 57.0078 16.9707 56.3672 17.4824C55.7266 17.9941 55.1094 18.2813 54.5156 18.3438L54.4336 18.3438C54.1289 18.3438 53.8281 18.2441 53.5313 18.0449C53.2344 17.8457 53.0859 17.4961 53.0859 16.9961C53.0859 16.2539 53.2852 15.5352 53.6836 14.8398C53.8242 14.5977 54.0684 14.1875 54.416 13.6094C54.7637 13.0313 55.0859 12.4727 55.3828 11.9336C55.6641 11.5039 55.9531 11.0039 56.25 10.4336C56.5469 9.86328 56.7734 9.45508 56.9297 9.20898C57.0859 8.96289 57.2344 8.83985 57.375 8.83984C57.5156 8.83985 57.6973 8.92774 57.9199 9.10352C58.1426 9.2793 58.2539 9.48828 58.2539 9.73047C58.2539 9.97266 57.7422 11.0234 56.7188 12.8828C55.6953 14.7422 55.1836 15.8008 55.1836 16.0586C55.1836 16.1211 55.2148 16.1523 55.2773 16.1523C55.4883 16.1523 55.9551 15.8535 56.6777 15.2559C57.4004 14.6582 58.1992 13.8906 59.0742 12.9531C60.0508 11.9141 60.8164 10.9648 61.3711 10.1055C61.4414 9.99609 61.498 9.91211 61.541 9.85352C61.584 9.79492 61.6387 9.7168 61.7051 9.61914C61.7715 9.52148 61.8262 9.44531 61.8691 9.39063C61.9121 9.33594 61.9629 9.27148 62.0215 9.19727C62.0801 9.12305 62.1328 9.06641 62.1797 9.02734C62.2266 8.98828 62.2773 8.94922 62.332 8.91016C62.418 8.83203 62.5137 8.79297 62.6191 8.79297C62.7246 8.79297 62.8848 8.81641 63.0996 8.86328C63.3145 8.91016 63.4434 8.96094 63.4863 9.01563C63.5293 9.07031 63.5742 9.12109 63.6211 9.16797C63.6992 9.25391 63.7383 9.33398 63.7383 9.4082C63.7383 9.48242 63.707 9.58008 63.6445 9.70117C63.582 9.82227 63.2188 10.4414 62.5547 11.5586C60.6016 14.8555 59.2578 17.2617 58.5234 18.7773C59.7109 17.7773 61.3828 16.2969 63.5391 14.3359C63.625 14.2656 63.7109 14.2305 63.7969 14.2305C64.0391 14.2305 64.1602 14.6016 64.1602 15.3438C64.1602 15.5625 64.1211 15.707 64.043 15.7773C61.707 17.5742 59.5078 19.582 57.4453 21.8008L57.4453 21.8008ZM53.0625 9.00391C53.6563 9.00391 53.9531 9.20703 53.9531 9.61328C53.9531 9.83203 53.9023 9.99219 53.8008 10.0938C53.6992 10.1953 53.4824 10.5332 53.1504 11.1074C52.8184 11.6816 52.4512 12.416 52.0488 13.3105C51.6465 14.2051 51.3867 14.9688 51.2695 15.6016C51.2461 15.6797 51.2344 15.7891 51.2344 15.9297C51.2344 16.3594 51.3516 16.5742 51.5859 16.5742C51.8203 16.5742 52.0977 16.4629 52.418 16.2402C52.7383 16.0176 53.0234 15.7793 53.2734 15.5254C53.5234 15.2715 53.7676 15.0117 54.0059 14.7461C54.2441 14.4805 54.3867 14.3281 54.4336 14.2891C54.4805 14.25 54.5508 14.2305 54.6445 14.2305C54.793 14.2305 54.8828 14.3438 54.9141 14.5703C54.9453 14.7969 54.9609 15.0313 54.9609 15.2734C54.9609 15.5156 54.9375 15.6836 54.8906 15.7773C54.6641 16.2227 54.166 16.7578 53.3965 17.3828C52.627 18.0078 51.9277 18.3203 51.2988 18.3203C50.6699 18.3203 50.207 18.1289 49.9102 17.7461C49.6133 17.3633 49.4648 16.9395 49.4648 16.4746C49.4648 16.0098 49.543 15.6016 49.6992 15.25C49.0508 16.0938 48.3066 16.8086 47.4668 17.3945C46.627 17.9805 45.9414 18.2734 45.4102 18.2734C44.8789 18.2734 44.4453 18.1133 44.1094 17.793C43.6797 17.3945 43.4648 16.8418 43.4648 16.1348C43.4648 15.4277 43.623 14.7168 43.9395 14.002C44.2559 13.2871 44.6719 12.6348 45.1875 12.0449C45.7031 11.4551 46.2695 10.918 46.8867 10.4336C48.1836 9.41797 49.3789 8.91016 50.4727 8.91016C50.8945 8.91016 51.2383 8.98047 51.5039 9.12109C51.7695 9.26172 51.9375 9.4375 52.0078 9.64844C52.1953 9.36719 52.3535 9.18945 52.4824 9.11523C52.6113 9.04102 52.8047 9.00391 53.0625 9.00391L53.0625 9.00391ZM51.5039 10.3047C50.168 10.3047 48.7852 11.0469 47.3555 12.5313C46.8477 13.0625 46.3809 13.6992 45.9551 14.4414C45.5293 15.1836 45.3164 15.6914 45.3164 15.9648C45.3164 16.2383 45.3945 16.375 45.5508 16.375C45.7695 16.375 46.3789 15.9434 47.3789 15.0801C48.3789 14.2168 49.043 13.582 49.3711 13.1758C49.7305 12.7227 50.4414 11.7656 51.5039 10.3047L51.5039 10.3047ZM35.4844 18.1914L34.7344 18.0273C34.5078 18.0039 34.3203 17.8594 34.1719 17.5938C34.1328 17.5234 34.1074 17.4805 34.0957 17.4648C34.084 17.4492 34.0781 17.4297 34.0781 17.4063C34.0781 17.3828 34.0859 17.3496 34.1016 17.3066C34.1172 17.2637 34.1406 17.2051 34.1719 17.1309C34.2031 17.0566 34.2305 16.9805 34.2539 16.9023C34.418 16.3477 35.7891 13.7383 38.3672 9.07422C40.9453 4.41016 42.2949 2.00586 42.416 1.86133C42.5371 1.7168 42.627 1.61133 42.6855 1.54492C42.7441 1.47852 42.8105 1.42188 42.8848 1.375C42.959 1.32813 43.0137 1.29688 43.0488 1.28125C43.084 1.26563 43.1543 1.24024 43.2598 1.20508C43.3652 1.16992 43.4746 1.13281 43.5879 1.09375C43.7012 1.05469 43.8203 1.00195 43.9453 0.935547C44.0703 0.869141 44.1563 0.822266 44.2031 0.794922C44.25 0.767579 44.3105 0.753908 44.3848 0.753906C44.459 0.753906 44.5293 0.789063 44.5957 0.859375C44.6621 0.929688 44.6953 1.01953 44.6953 1.12891L44.6953 1.21094C44.6563 1.35938 43.6387 3.21094 41.6426 6.76563C39.6465 10.3203 38.5586 12.2656 38.3789 12.6016C38.9883 12.3594 39.8867 11.7617 41.0742 10.8086C42.2617 9.85547 42.8613 9.22266 42.873 8.91016C42.8848 8.59766 42.9922 8.37109 43.1953 8.23047C43.3984 8.08984 43.6191 8.01953 43.8574 8.01953C44.0957 8.01953 44.2988 8.09961 44.4668 8.25977C44.6348 8.41992 44.7148 8.68164 44.707 9.04492C44.6992 9.4082 44.4961 9.86914 44.0977 10.4277C43.6992 10.9863 43.2227 11.5039 42.668 11.9805C41.4727 12.9961 40.4766 13.668 39.6797 13.9961C40.3125 15.707 41.0391 16.5625 41.8594 16.5625C42.3047 16.5 42.8516 16.1367 43.5 15.4727C43.7422 15.2305 43.9766 14.9863 44.2031 14.7402C44.4297 14.4941 44.5684 14.3477 44.6191 14.3008C44.6699 14.2539 44.7422 14.2305 44.8359 14.2305C44.9844 14.2305 45.0742 14.3438 45.1055 14.5703C45.1367 14.7969 45.1523 15.0313 45.1523 15.2734C45.1523 15.5156 45.1289 15.6836 45.082 15.7773C44.8555 16.2148 44.3594 16.748 43.5938 17.377C42.8281 18.0059 42.1016 18.3203 41.4141 18.3203C40.9219 18.3203 40.5313 18.2266 40.2422 18.0391C39.9531 17.8516 39.7266 17.6953 39.5625 17.5703C39.3984 17.4453 39.2305 17.2754 39.0586 17.0605C38.8867 16.8457 38.7441 16.668 38.6309 16.5273C38.5176 16.3867 38.3887 16.1914 38.2441 15.9414C38.0996 15.6914 37.9961 15.5137 37.9336 15.4082C37.8711 15.3027 37.7793 15.1309 37.6582 14.8926C37.5371 14.6543 37.4688 14.5234 37.4531 14.5C37.3125 14.7891 37.1563 15.1016 36.9844 15.4375C36.8125 15.7734 36.6563 16.0859 36.5156 16.375C36.375 16.6641 36.2461 16.9277 36.1289 17.166C36.0117 17.4043 35.916 17.6016 35.8418 17.7578C35.7676 17.9141 35.7266 17.9961 35.7188 18.0039C35.6641 18.1289 35.5859 18.1914 35.4844 18.1914L35.4844 18.1914ZM34.4648 8.98047C34.6992 8.98047 34.9395 9.08594 35.1855 9.29688C35.4316 9.50781 35.5547 9.83008 35.5547 10.2637C35.5547 10.6973 35.4023 11.2598 35.0977 11.9512C34.793 12.6426 34.375 13.3457 33.8438 14.0605C33.3125 14.7754 32.7285 15.4473 32.0918 16.0762C31.4551 16.7051 30.7676 17.2207 30.0293 17.623C29.291 18.0254 28.6211 18.2266 28.0195 18.2266C27.418 18.2266 26.9355 18.0371 26.5723 17.6582C26.209 17.2793 26.0273 16.752 26.0273 16.0762C26.0273 15.4004 26.1934 14.6523 26.5254 13.832C26.8574 13.0117 27.2852 12.2871 27.8086 11.6582C28.332 11.0293 28.8984 10.4648 29.5078 9.96484C30.7813 8.94141 31.8984 8.42969 32.8594 8.42969C33.2891 8.42969 33.6133 8.53516 33.832 8.74609C33.9102 8.81641 33.9492 8.87891 33.9492 8.93359L33.9492 9.00391C34.0586 8.98828 34.2305 8.98047 34.4648 8.98047L34.4648 8.98047ZM33.2813 10.5391L33.1992 10.5391C32.9258 10.5391 32.7891 10.3398 32.7891 9.94141C32.2188 10.1289 31.6211 10.457 30.9961 10.9258C30.3711 11.3945 29.8125 11.9492 29.3203 12.5898C28.3203 13.8711 27.8125 14.9531 27.7969 15.8359C27.7969 16.2734 27.957 16.4922 28.2773 16.4922C28.5977 16.4922 29.0742 16.2813 29.707 15.8594C30.3398 15.4375 30.9668 14.8359 31.5879 14.0547C32.209 13.2734 32.6738 12.5957 32.9824 12.0215C33.291 11.4473 33.4453 11.0566 33.4453 10.8496C33.4453 10.6426 33.3906 10.5391 33.2813 10.5391L33.2813 10.5391ZM25.6758 8.98047C25.9102 8.98047 26.1504 9.08594 26.3965 9.29688C26.6426 9.50781 26.7656 9.83008 26.7656 10.2637C26.7656 10.6973 26.6133 11.2598 26.3086 11.9512C26.0039 12.6426 25.5859 13.3457 25.0547 14.0605C24.5234 14.7754 23.9395 15.4473 23.3027 16.0762C22.666 16.7051 21.9785 17.2207 21.2402 17.623C20.502 18.0254 19.832 18.2266 19.2305 18.2266C18.6289 18.2266 18.1465 18.0371 17.7832 17.6582C17.4199 17.2793 17.2383 16.752 17.2383 16.0762C17.2383 15.4004 17.4043 14.6523 17.7363 13.832C18.0684 13.0117 18.4961 12.2871 19.0195 11.6582C19.543 11.0293 20.1094 10.4648 20.7188 9.96484C21.9922 8.94141 23.1094 8.42969 24.0703 8.42969C24.5 8.42969 24.8242 8.53516 25.043 8.74609C25.1211 8.81641 25.1602 8.87891 25.1602 8.93359L25.1602 9.00391C25.2695 8.98828 25.4414 8.98047 25.6758 8.98047L25.6758 8.98047ZM24.4922 10.5391L24.4102 10.5391C24.1367 10.5391 24 10.3398 24 9.94141C23.4297 10.1289 22.832 10.457 22.207 10.9258C21.582 11.3945 21.0234 11.9492 20.5313 12.5898C19.5313 13.8711 19.0234 14.9531 19.0078 15.8359C19.0078 16.2734 19.168 16.4922 19.4883 16.4922C19.8086 16.4922 20.2852 16.2813 20.918 15.8594C21.5508 15.4375 22.1777 14.8359 22.7988 14.0547C23.4199 13.2734 23.8848 12.5957 24.1934 12.0215C24.502 11.4473 24.6563 11.0566 24.6563 10.8496C24.6563 10.6426 24.6016 10.5391 24.4922 10.5391L24.4922 10.5391Z"
                        , id "ookay"
                        , fill "currentColor"
                        , stroke "none"
                        ]
                        []
                    ]
                ]